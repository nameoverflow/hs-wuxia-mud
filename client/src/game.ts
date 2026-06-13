import { writable } from "svelte/store";
import { hasTranslation, translate, type Locale } from "./i18n";
import type {
  ActiveSkillFailureReason,
  ActiveSkillSummary,
  ArtSummary,
  BattleSnapshot,
  CombatMessage,
  Direction,
  EffectSummary,
  InventoryItemSummary,
  LoginEvent,
  NetPlayerAction,
  PlayerAction,
  QuestLogEntry,
  RewardSummary,
  RoomCharacterSummary,
  RoomExitSummary,
  ServerMessage
} from "./protocol";

export interface PlayerStats {
  hp: number;
  maxHp: number;
  qi: number;
  maxQi: number;
  ap: number;
}

export interface MessageEntry {
  id: number;
  time: string;
  type: "system" | "move" | "combat" | "skill" | "dialogue" | "story" | "say" | "error" | "reward";
  text: string;
}

export interface RoomState {
  name: string;
  desc: string;
  characters: RoomCharacterSummary[];
  exits: RoomExitSummary[];
}

export interface BattleState {
  active: boolean;
  player: BattleSnapshot["battleSnapshotPlayer"] | null;
  enemy: BattleSnapshot["battleSnapshotEnemy"] | null;
  cooldowns: Record<string, number>;
  activeSkills: ActiveSkillSummary[];
}

export interface GameState {
  locale: Locale;
  connected: boolean;
  connecting: boolean;
  username: string;
  playerStatus: string;
  money: number;
  stats: PlayerStats;
  room: RoomState;
  effects: EffectSummary[];
  inventory: InventoryItemSummary[];
  quests: QuestLogEntry[];
  arts: ArtSummary[];
  battle: BattleState;
  messages: MessageEntry[];
  lastError: string | null;
}

const initialState: GameState = {
  locale: "zh",
  connected: false,
  connecting: false,
  username: "",
  playerStatus: "normal",
  money: 0,
  stats: { hp: 100, maxHp: 100, qi: 100, maxQi: 100, ap: 0 },
  room: { name: "", desc: "", characters: [], exits: [] },
  effects: [],
  inventory: [],
  quests: [],
  arts: [],
  battle: { active: false, player: null, enemy: null, cooldowns: {}, activeSkills: [] },
  messages: [{ id: 1, time: now(), type: "system", text: translate("zh", "message.initial") }],
  lastError: null
};

let ws: WebSocket | null = null;
let messageId = 1;
let reconnectTimer: number | null = null;

export const game = writable<GameState>(initialState);

function now() {
  return new Date().toLocaleTimeString([], { hour: "2-digit", minute: "2-digit", second: "2-digit" });
}

function withLocale(fn: (locale: Locale) => string) {
  let result = "";
  game.update((state) => {
    result = fn(state.locale);
    return state;
  });
  return result;
}

function addMessage(type: MessageEntry["type"], text: string) {
  game.update((state) => ({
    ...state,
    messages: [...state.messages.slice(-179), { id: ++messageId, time: now(), type, text }]
  }));
}

function t(locale: Locale, key: string, values: Record<string, unknown> = {}) {
  return translate(locale, key, values);
}

export function setLocale(locale: Locale) {
  game.update((state) => ({ ...state, locale }));
}

export function clearMessages() {
  game.update((state) => ({ ...state, messages: [] }));
}

export function connect(username: string, options: { reset?: boolean } = {}) {
  const cleanName = username.trim();
  if (!cleanName) {
    addMessage("error", withLocale((locale) => t(locale, "error.username_required")));
    return;
  }

  if (ws && (ws.readyState === WebSocket.OPEN || ws.readyState === WebSocket.CONNECTING)) {
    return;
  }

  if (reconnectTimer !== null) {
    window.clearTimeout(reconnectTimer);
    reconnectTimer = null;
  }

  game.update((state) => ({ ...state, username: cleanName, connecting: true, lastError: null }));
  addMessage("system", withLocale((locale) => t(locale, "connection.connecting", { user: cleanName })));

  ws = new WebSocket("ws://127.0.0.1:9160");

  ws.addEventListener("open", () => {
    const event: LoginEvent = {
      tag: "Login",
      username: cleanName,
      password: options.reset ? "__dev_reset" : ""
    };
    ws?.send(JSON.stringify(event));
    game.update((state) => ({
      ...state,
      connected: true,
      connecting: false,
      username: cleanName
    }));
    addMessage("system", withLocale((locale) => t(locale, "connection.ready")));
    window.setTimeout(() => {
      sendAction({ other: "view" });
      sendAction({ other: "quests" });
      sendAction({ other: "inventory" });
      sendAction({ other: "arts" });
    }, 250);
  });

  ws.addEventListener("message", (event) => {
    try {
      processServerMessage(JSON.parse(String(event.data)) as ServerMessage);
    } catch {
      addMessage("system", String(event.data));
    }
  });

  ws.addEventListener("error", () => {
    addMessage("error", withLocale((locale) => t(locale, "connection.error")));
  });

  ws.addEventListener("close", () => {
    game.update((state) => ({
      ...state,
      connected: false,
      connecting: false,
      battle: { ...state.battle, active: false }
    }));
    addMessage("system", withLocale((locale) => t(locale, "connection.closed")));
    ws = null;
  });
}

export function disconnect() {
  ws?.close();
  ws = null;
}

export function sendAction(action: PlayerAction) {
  if (!ws || ws.readyState !== WebSocket.OPEN) return;
  const event: NetPlayerAction = { tag: "NetPlayerAction", contents: action };
  ws.send(JSON.stringify(event));
}

export function processServerMessage(message: ServerMessage | { tag: string; contents?: unknown }) {
  switch (message.tag) {
    case "MoveMsg":
      handleMove(message.contents as string);
      break;
    case "ViewMsg":
      handleView(message.contents as [string, string, unknown[], unknown[]]);
      break;
    case "AttackMsg":
      handleAttack(message.contents as [string, string]);
      break;
    case "CombatNormalMsg":
      handleCombatNormal(message.contents as [string, string, CombatMessage, number]);
      break;
    case "CombatSettlementMsg":
      handleCombatSettlement(message.contents as [string, string, boolean]);
      break;
    case "ActiveSkillMsg":
      handleActiveSkill(message.contents as [string, string, CombatMessage]);
      break;
    case "ActiveSkillFailureMsg":
      handleActiveSkillFailure(message.contents as ActiveSkillFailureReason);
      break;
    case "BattleStateMsg":
      handleBattleState(message.contents as BattleSnapshot);
      break;
    case "PlayerStatsMsg":
      handlePlayerStats(message.contents as [number, number, number, number, number, string]);
      break;
    case "QuestLogMsg":
      game.update((state) => ({ ...state, quests: (message.contents as QuestLogEntry[]) || [] }));
      break;
    case "InventoryMsg":
      {
        const [money, inventory] = (message.contents as [number, InventoryItemSummary[]]) || [0, []];
      game.update((state) => ({
        ...state,
        money: money || 0,
        inventory: inventory || []
      }));
      }
      break;
    case "ArtsMsg":
      game.update((state) => ({ ...state, arts: (message.contents as ArtSummary[]) || [] }));
      break;
    case "RewardMsg":
      handleReward((message.contents as RewardSummary[]) || []);
      break;
    case "UseItemMsg":
      addMessage("system", ((message.contents as [string, string]) || ["", ""])[1]);
      sendAction({ other: "inventory" });
      sendAction({ other: "arts" });
      break;
    case "DialogueMsg":
      {
        const [speaker, text] = (message.contents as [string, string]) || ["", ""];
        addMessage("dialogue", withLocale((locale) => t(locale, "message.dialogue", { speaker, text })));
      }
      break;
    case "StoryMsg":
      {
        const [speaker, text] = (message.contents as [string, string]) || ["", ""];
        addMessage("story", formatStoryMessage(speaker, text));
      }
      break;
    case "SayMsg":
      {
        const [speaker, text] = (message.contents as [string, string]) || ["", ""];
        addMessage("say", withLocale((locale) => t(locale, "message.say", { speaker, text })));
      }
      break;
    case "SystemMsg":
      handleSystem(message.contents as { systemMessageKey: string; systemMessageParams: Record<string, string> });
      break;
    case "ErrorMsg":
      handleError(message.contents as { errorSummaryCode: string; errorSummaryParams: Record<string, string> });
      break;
    default:
      addMessage("system", JSON.stringify(message));
  }
}

function handleMove(room: string) {
  game.update((state) => ({
    ...state,
    room: { ...state.room, name: room }
  }));
  addMessage("move", withLocale((locale) => t(locale, "message.move", { room })));
}

function handleView(contents: [string, string, unknown[], unknown[]]) {
  const [name, desc, chars, exits] = contents;
  game.update((state) => ({
    ...state,
    room: {
      name,
      desc,
      characters: normalizeCharacters(chars),
      exits: normalizeExits(exits)
    }
  }));
}

function handleAttack([attacker, defender]: [string, string]) {
  game.update((state) => ({
    ...state,
    battle: { ...state.battle, active: true }
  }));
  addMessage("combat", withLocale((locale) => t(locale, "message.attack", { attacker, defender })));
}

function handleCombatNormal([attacker, defender, combatMessage, damage]: [string, string, CombatMessage, number]) {
  const action = withLocale((locale) => formatCombatMessage(locale, combatMessage));
  addMessage("combat", withLocale((locale) => t(locale, "message.combat.damage", { attacker, defender, action, damage })));
}

function handleCombatSettlement([, enemy, won]: [string, string, boolean]) {
  addMessage("combat", withLocale((locale) => t(locale, won ? "message.combat.victory" : "message.combat.defeat", { enemy })));
  game.update((state) => ({
    ...state,
    battle: { ...state.battle, active: false, enemy: null, activeSkills: [], cooldowns: {} }
  }));
  sendAction({ other: "view" });
  sendAction({ other: "quests" });
  sendAction({ other: "inventory" });
  sendAction({ other: "arts" });
}

function handleActiveSkill([caster, target, combatMessage]: [string, string, CombatMessage]) {
  const action = withLocale((locale) => formatCombatMessage(locale, combatMessage));
  addMessage("skill", withLocale((locale) => t(locale, "message.active_skill", { caster, target, action })));
}

function handleActiveSkillFailure(reason: ActiveSkillFailureReason) {
  const text = withLocale((locale) => formatActiveSkillFailure(locale, reason));
  addMessage("error", withLocale((locale) => t(locale, "message.active_skill_failed", { reason: text })));
}

function handleBattleState(snapshot: BattleSnapshot) {
  const cooldowns = Object.fromEntries(
    (snapshot.battleSnapshotActiveSkillCooldowns || []).map((cooldown) => [
      cooldown.activeSkillCooldownSummaryActiveSkillId,
      cooldown.activeSkillCooldownSummaryRemaining
    ])
  );

  game.update((state) => ({
    ...state,
    stats: {
      hp: snapshot.battleSnapshotPlayer.combatantSnapshotHp,
      maxHp: snapshot.battleSnapshotPlayer.combatantSnapshotMaxHp,
      qi: snapshot.battleSnapshotPlayer.combatantSnapshotQi,
      maxQi: snapshot.battleSnapshotPlayer.combatantSnapshotMaxQi,
      ap: snapshot.battleSnapshotPlayer.combatantSnapshotAp
    },
    effects: snapshot.battleSnapshotPlayer.combatantSnapshotEffects || [],
    battle: {
      active: true,
      player: snapshot.battleSnapshotPlayer,
      enemy: snapshot.battleSnapshotEnemy,
      cooldowns,
      activeSkills: snapshot.battleSnapshotActiveSkills || []
    }
  }));
}

function handlePlayerStats([hp, maxHp, qi, maxQi, ap, status]: [number, number, number, number, number, string]) {
  game.update((state) => ({
    ...state,
    playerStatus: status,
    stats: { hp, maxHp, qi, maxQi, ap },
    battle: { ...state.battle, active: status === "in_battle" || state.battle.active }
  }));
}

function handleReward(rewards: RewardSummary[]) {
  if (!rewards.length) return;
  addMessage("reward", withLocale((locale) => t(locale, "quest.reward", { value: rewards.map(formatReward).join(", ") })));
  if (rewards.some((reward) => reward.rewardSummaryKind === "martial_art")) {
    sendAction({ other: "arts" });
  }
}

function handleSystem(contents: { systemMessageKey: string; systemMessageParams: Record<string, string> }) {
  const key = `system.${contents?.systemMessageKey || "unknown"}`;
  addMessage(
    "system",
    withLocale((locale) =>
      hasTranslation(locale, key)
        ? t(locale, key, contents?.systemMessageParams || {})
        : t(locale, "system.unknown", { code: contents?.systemMessageKey || "unknown" })
    )
  );
}

function handleError(contents: { errorSummaryCode: string; errorSummaryParams: Record<string, string> }) {
  const code = contents?.errorSummaryCode || "unknown";
  const params = contents?.errorSummaryParams || {};
  const key = `error.${code}`;
  const text = withLocale((locale) =>
    hasTranslation(locale, key) ? t(locale, key, localizeParams(locale, params)) : t(locale, "error.unknown", { code })
  );
  game.update((state) => ({ ...state, lastError: text }));
  addMessage("error", text);
}

function normalizeCharacters(chars: unknown[]): RoomCharacterSummary[] {
  return (chars || []).map((char) => {
    if (char && typeof char === "object") {
      const obj = char as Record<string, unknown>;
      return {
        id: typeof obj.id === "string" ? obj.id : null,
        name: String(obj.name || obj.id || "Unknown"),
        desc: String(obj.desc || ""),
        actions: Array.isArray(obj.actions) ? obj.actions.map((action) => String(action).toLowerCase()) : []
      };
    }
    const text = String(char || "");
    return { id: null, name: text, desc: "", actions: [] };
  });
}

function normalizeExits(exits: unknown[]): RoomExitSummary[] {
  return (exits || [])
    .map((exit) => {
      if (typeof exit === "string") {
        return { direction: normalizeDirection(exit), roomId: null, roomName: null, position: null };
      }
      if (!exit || typeof exit !== "object") return null;
      const obj = exit as Record<string, unknown>;
      return {
        direction: normalizeDirection(String(obj.direction || obj.roomExitSummaryDirection || "")),
        roomId: typeof obj.roomId === "string" ? obj.roomId : typeof obj.roomExitSummaryRoomId === "string" ? obj.roomExitSummaryRoomId : null,
        roomName: typeof obj.roomName === "string" ? obj.roomName : typeof obj.roomExitSummaryRoomName === "string" ? obj.roomExitSummaryRoomName : null,
        position: (obj.position || obj.roomExitSummaryPosition || null) as RoomExitSummary["position"]
      };
    })
    .filter((exit): exit is RoomExitSummary => Boolean(exit?.direction));
}

export function normalizeDirection(direction: string): Direction {
  const map: Record<string, Direction> = {
    north: "North",
    south: "South",
    east: "East",
    west: "West",
    northeast: "NorthEast",
    northwest: "NorthWest",
    southeast: "SouthEast",
    southwest: "SouthWest"
  };
  return map[direction.toLowerCase()] || (direction as Direction);
}

export function directionVector(direction: Direction) {
  const vectors: Record<Direction, { x: number; y: number }> = {
    North: { x: 0, y: -1 },
    South: { x: 0, y: 1 },
    East: { x: 1, y: 0 },
    West: { x: -1, y: 0 },
    NorthEast: { x: 1, y: -1 },
    NorthWest: { x: -1, y: -1 },
    SouthEast: { x: 1, y: 1 },
    SouthWest: { x: -1, y: 1 }
  };
  return vectors[direction] || { x: 0, y: 0 };
}

export function exitLabel(locale: Locale, exit: RoomExitSummary) {
  return exit.roomName || t(locale, `direction.${exit.direction}`);
}

export function percent(value: number, max: number) {
  if (!max || max <= 0) return 0;
  return Math.max(0, Math.min(100, (value / max) * 100));
}

export function formatStatus(locale: Locale, status: string) {
  return t(locale, `status.${status}`) || status;
}

export function formatArtType(locale: Locale, type: string) {
  return t(locale, `art.type.${type}`) || type;
}

export function formatReward(reward: RewardSummary) {
  if (reward.rewardSummaryKind === "money") return `${reward.rewardSummaryAmount} ${reward.rewardSummaryName}`;
  if (reward.rewardSummaryKind === "martial_art") return reward.rewardSummaryName;
  return `${reward.rewardSummaryName} x${reward.rewardSummaryAmount}`;
}

export function skillAvailability(state: GameState, skill: ActiveSkillSummary) {
  const cooldown = state.battle.cooldowns[skill.activeSkillSummaryId] || 0;
  const missing = (skill.activeSkillSummaryReqStatus || []).filter(
    (effect) => !state.effects.some((active) => active.effectSummaryId === effect)
  );
  if (cooldown > 0) return { ready: false, reason: "cooldown", label: translate(state.locale, "active_skill.cooldown", { seconds: Math.ceil(cooldown) }) };
  if (missing.length) {
    const labels = missing.map((effectId) => {
      const index = skill.activeSkillSummaryReqStatus.indexOf(effectId);
      return skill.activeSkillSummaryReqStatusNames[index] || effectId;
    });
    return { ready: false, reason: "requires", label: translate(state.locale, "active_skill.requires", { value: labels.join(", ") }) };
  }
  if (state.stats.qi < skill.activeSkillSummaryCost) return { ready: false, reason: "qi", label: translate(state.locale, "active_skill.need_qi") };
  if (state.stats.ap < skill.activeSkillSummaryApReq) return { ready: false, reason: "ap", label: translate(state.locale, "active_skill.need_ap") };
  return { ready: true, reason: "ready", label: translate(state.locale, "active_skill.ready") };
}

function formatCombatMessage(locale: Locale, combatMessage: CombatMessage) {
  if (!combatMessage) return "";
  if (combatMessage.kind === "script") return combatMessage.text || "";
  if (combatMessage.kind === "effect_tick") {
    const effect = combatMessage.effectName || combatMessage.effectId;
    const key = `message.combat.effect.${combatMessage.effectKind}`;
    return hasTranslation(locale, key)
      ? t(locale, key, { effect, amount: combatMessage.amount })
      : t(locale, "message.combat.effect.unknown", { effect, amount: combatMessage.amount });
  }
  return "";
}

function formatStoryMessage(speaker: string, text: string) {
  const normalizedSpeaker = speaker.trim().toLowerCase();
  if (!normalizedSpeaker || normalizedSpeaker === "旁白" || normalizedSpeaker === "narrator") {
    return text || speaker;
  }
  return text ? `${speaker}：${text}` : speaker;
}

function formatActiveSkillFailure(locale: Locale, reason: ActiveSkillFailureReason) {
  switch (reason.reason) {
    case "need_ap":
      return t(locale, "active_skill.failure.need_ap", reason);
    case "need_qi":
      return t(locale, "active_skill.failure.need_qi", reason);
    case "cooldown":
      return t(locale, "active_skill.failure.cooldown", { seconds: Math.ceil(reason.remaining) });
    case "missing_status":
      return t(locale, "active_skill.failure.missing_status", { value: reason.statuses.join(", ") });
    case "unavailable":
      return t(locale, "active_skill.failure.unavailable", { activeSkill: reason.activeSkillId });
    default:
      return t(locale, "active_skill.failure.unknown");
  }
}

function localizeParams(locale: Locale, params: Record<string, string>) {
  const result = { ...params };
  if (result.direction) result.direction = t(locale, `direction.${normalizeDirection(result.direction)}`);
  if (result.action) {
    const key = `action.${result.action}`;
    result.action = hasTranslation(locale, key) ? t(locale, key) : result.action;
  }
  return result;
}

export function testEntryFromUrl() {
  const params = new URLSearchParams(window.location.search);
  if (params.get("test") !== "1") return null;
  return {
    user: params.get("user") || "tester",
    reset: params.get("reset") !== "0"
  };
}
