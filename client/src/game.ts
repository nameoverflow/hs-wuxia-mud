import { writable } from "svelte/store";
import { resolveCombatTimeline, resolveSettlementTimeline } from "./battle/animationResolver";
import { combatStyleFromSnapshot, visualProfileFromGender } from "./battle/battleActionCatalog";
import type { BattleSide, ResolvedBattleTimeline } from "./battle/animationTypes";
export type { BattleSide } from "./battle/animationTypes";
import { hasTranslation, translate, type Locale } from "./i18n";
import type {
  ActiveSkillFailureReason,
  ActiveSkillSummary,
  ArtSummary,
  BattleSnapshot,
  CharacterCreationChoice,
  CharacterCreationConfig,
  CombatEvent,
  CombatMessage,
  Direction,
  EffectSummary,
  InventoryItemSummary,
  LoginEvent,
  MapOverviewSummary,
  NetPlayerAction,
  PlayerAction,
  PlayerStatsPayload,
  QuestLogEntry,
  RewardSummary,
  RoomPosition,
  RoomCharacterSummary,
  RoomExitSummary,
  ServerMessage
} from "./protocol";

const websocketUrl = "ws://127.0.0.1:9160";

export interface PlayerStats {
  hp: number;
  maxHp: number;
  qi: number;
  maxQi: number;
  jing: number;
  maxJing: number;
  ap: number;
  gender: string;
  appearance: number;
  appearanceText: string;
  portraitKey: string;
  strength: number;
  agility: number;
  vitality: number;
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
  animation: BattleAnimationState;
  apSyncedAt: number;
  actionLockUntil: number;
}

export interface BattleAnimationState {
  activeTimeline: ResolvedBattleTimeline | null;
  queueDepth: number;
}

export interface StoryTransitionState {
  active: boolean;
  text: string;
  durationMs: number;
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
  mapOverview: MapOverviewSummary | null;
  effects: EffectSummary[];
  inventory: InventoryItemSummary[];
  quests: QuestLogEntry[];
  arts: ArtSummary[];
  battle: BattleState;
  storyTransition: StoryTransitionState;
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
  stats: {
    hp: 100,
    maxHp: 100,
    qi: 100,
    maxQi: 100,
    jing: 120,
    maxJing: 120,
    ap: 0,
    gender: "unknown",
    appearance: 5,
    appearanceText: "",
    portraitKey: "beauty-score-05",
    strength: 18,
    agility: 18,
    vitality: 18
  },
  room: { name: "", desc: "", characters: [], exits: [] },
  mapOverview: null,
  effects: [],
  inventory: [],
  quests: [],
  arts: [],
  battle: {
    active: false,
    player: null,
    enemy: null,
    cooldowns: {},
    activeSkills: [],
    animation: { activeTimeline: null, queueDepth: 0 },
    apSyncedAt: 0,
    actionLockUntil: 0
  },
  storyTransition: { active: false, text: "", durationMs: 1000 },
  messages: [{ id: 1, time: now(), type: "system", text: translate("zh", "message.initial") }],
  lastError: null
};

let ws: WebSocket | null = null;
let messageId = 1;
let reconnectTimer: number | null = null;
let latestState = initialState;

export const game = writable<GameState>(initialState);
game.subscribe((state) => {
  latestState = state;
});

interface QueuedBattleTimeline {
  timeline: ResolvedBattleTimeline;
  messageType: MessageEntry["type"];
  messageText: string;
  after?: () => void;
}

const battleTimelineQueue: QueuedBattleTimeline[] = [];
let battleAnimationId = 0;
let battleAnimationTimer: number | null = null;
const storyMessageQueue: (ServerMessage | { tag: string; contents?: unknown })[] = [];
let storyQueueProcessing = false;
let storyQueueToken = 0;
let storyTransitionTimer: number | null = null;

function now() {
  return new Date().toLocaleTimeString([], { hour: "2-digit", minute: "2-digit", second: "2-digit" });
}

function wait(ms: number) {
  return new Promise<void>((resolve) => {
    window.setTimeout(resolve, ms);
  });
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

function clearBattleAnimationQueue() {
  battleTimelineQueue.length = 0;
  if (battleAnimationTimer !== null) {
    window.clearTimeout(battleAnimationTimer);
    battleAnimationTimer = null;
  }
  game.update((state) => ({
    ...state,
    battle: {
      ...state.battle,
      animation: { activeTimeline: null, queueDepth: 0 }
    }
  }));
}

function clearStoryMessageQueue() {
  storyMessageQueue.length = 0;
  storyQueueProcessing = false;
  storyQueueToken += 1;
  if (storyTransitionTimer !== null) {
    window.clearTimeout(storyTransitionTimer);
    storyTransitionTimer = null;
  }
  game.update((state) => ({
    ...state,
    storyTransition: { active: false, text: "", durationMs: 1000 }
  }));
}

function normalizeStoryDuration(raw: unknown, fallback: number) {
  const parsed = typeof raw === "number" ? raw : Number(raw);
  if (!Number.isFinite(parsed)) return fallback;
  return Math.max(0, Math.min(8000, Math.round(parsed)));
}

function parseStoryTransition(contents: unknown) {
  if (Array.isArray(contents)) {
    return {
      text: String(contents[0] ?? ""),
      durationMs: normalizeStoryDuration(contents[1], 1000)
    };
  }
  if (contents && typeof contents === "object") {
    const value = contents as Record<string, unknown>;
    return {
      text: String(value.text ?? value.storyTransitionText ?? ""),
      durationMs: normalizeStoryDuration(value.ms ?? value.durationMs, 1000)
    };
  }
  return { text: "", durationMs: 1000 };
}

async function playStoryTransition(contents: unknown) {
  const { text, durationMs } = parseStoryTransition(contents);
  if (durationMs <= 0) return;
  if (storyTransitionTimer !== null) {
    window.clearTimeout(storyTransitionTimer);
  }
  game.update((state) => ({
    ...state,
    storyTransition: { active: true, text, durationMs }
  }));
  storyTransitionTimer = window.setTimeout(() => {
    storyTransitionTimer = null;
    game.update((state) => ({
      ...state,
      storyTransition: { active: false, text: "", durationMs: 1000 }
    }));
  }, durationMs);
  await wait(Math.min(280, Math.max(120, Math.floor(durationMs * 0.25))));
}

function enqueueServerMessage(message: ServerMessage | { tag: string; contents?: unknown }) {
  storyMessageQueue.push(message);
  void processStoryMessageQueue();
}

async function processStoryMessageQueue() {
  if (storyQueueProcessing) return;
  storyQueueProcessing = true;
  const token = storyQueueToken;
  try {
    while (storyMessageQueue.length > 0 && token === storyQueueToken) {
      const message = storyMessageQueue.shift();
      if (!message) continue;
      if (message.tag === "StoryDelayMsg") {
        await wait(normalizeStoryDuration(message.contents, 600));
        continue;
      }
      if (message.tag === "StoryTransitionMsg") {
        await playStoryTransition(message.contents);
        continue;
      }
      processServerMessage(message);
    }
  } finally {
    if (token === storyQueueToken) {
      storyQueueProcessing = false;
      if (storyMessageQueue.length > 0) void processStoryMessageQueue();
    }
  }
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

export function requestCharacterCreationConfig(): Promise<CharacterCreationConfig> {
  return new Promise((resolve, reject) => {
    const configWs = new WebSocket(websocketUrl);
    let settled = false;
    const timeout = window.setTimeout(() => {
      if (settled) return;
      settled = true;
      configWs.close();
      reject(new Error("character creation config request timed out"));
    }, 5000);

    const finish = (result: CharacterCreationConfig | Error) => {
      if (settled) return;
      settled = true;
      window.clearTimeout(timeout);
      configWs.close();
      if (result instanceof Error) {
        reject(result);
      } else {
        resolve(result);
      }
    };

    configWs.addEventListener("open", () => {
      configWs.send(JSON.stringify({ tag: "RequestCharacterCreationConfig" }));
    });

    configWs.addEventListener("message", (event) => {
      try {
        const message = JSON.parse(String(event.data)) as ServerMessage;
        if (message.tag === "CharacterCreationConfigMsg") {
          finish(message.contents);
        } else if (message.tag === "ErrorMsg") {
          finish(new Error(message.contents.errorSummaryCode));
        }
      } catch {
        finish(new Error("invalid character creation config response"));
      }
    });

    configWs.addEventListener("error", () => {
      finish(new Error("character creation config request failed"));
    });

    configWs.addEventListener("close", () => {
      if (!settled) {
        finish(new Error("character creation config connection closed"));
      }
    });
  });
}

export function connect(username: string, options: { reset?: boolean; creation?: CharacterCreationChoice | null } = {}) {
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
  clearBattleAnimationQueue();
  clearStoryMessageQueue();

  game.update((state) => ({ ...state, username: cleanName, connecting: true, lastError: null }));
  addMessage("system", withLocale((locale) => t(locale, "connection.connecting", { user: cleanName })));

  ws = new WebSocket(websocketUrl);

  ws.addEventListener("open", () => {
    const event: LoginEvent = {
      tag: "Login",
      username: cleanName,
      password: options.reset ? "__dev_reset" : "",
      creation: options.creation ?? null
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
      sendAction({ other: "map" });
    }, 250);
  });

  ws.addEventListener("message", (event) => {
    try {
      enqueueServerMessage(JSON.parse(String(event.data)) as ServerMessage);
    } catch {
      addMessage("system", String(event.data));
    }
  });

  ws.addEventListener("error", () => {
    addMessage("error", withLocale((locale) => t(locale, "connection.error")));
  });

  ws.addEventListener("close", () => {
    clearBattleAnimationQueue();
    clearStoryMessageQueue();
    game.update((state) => ({
      ...state,
      connected: false,
      connecting: false,
      battle: { ...state.battle, active: false, animation: { activeTimeline: null, queueDepth: 0 }, actionLockUntil: 0 }
    }));
    addMessage("system", withLocale((locale) => t(locale, "connection.closed")));
    ws = null;
  });
}

export function disconnect() {
  clearBattleAnimationQueue();
  clearStoryMessageQueue();
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
    case "MapOverviewMsg":
      handleMapOverview(message.contents);
      break;
    case "AttackMsg":
      handleAttack(message.contents as [string, string]);
      break;
    case "CombatEventMsg":
      handleCombatEvent(message.contents as CombatEvent);
      break;
    case "CombatSettlementMsg":
      handleCombatSettlement(message.contents as [string, string, boolean]);
      break;
    case "ActiveSkillFailureMsg":
      handleActiveSkillFailure(message.contents as ActiveSkillFailureReason);
      break;
    case "BattleStateMsg":
      handleBattleState(message.contents as BattleSnapshot);
      break;
    case "PlayerStatsMsg":
      handlePlayerStats(message.contents as PlayerStatsPayload);
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
    case "CharacterCreationConfigMsg":
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
  const shouldRefreshMap = Boolean(latestState.mapOverview);
  game.update((state) => ({
    ...state,
    room: { ...state.room, name: room }
  }));
  addMessage("move", withLocale((locale) => t(locale, "message.move", { room })));
  if (shouldRefreshMap) sendAction({ other: "map" });
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

function handleMapOverview(contents: unknown) {
  const mapOverview = normalizeMapOverview(contents);
  if (!mapOverview) return;
  game.update((state) => ({ ...state, mapOverview }));
}

function handleAttack([attacker, defender]: [string, string]) {
  game.update((state) => ({
    ...state,
    battle: { ...state.battle, active: true }
  }));
  addMessage("combat", withLocale((locale) => t(locale, "message.attack", { attacker, defender })));
}

function handleCombatEvent(event: CombatEvent) {
  const text = withLocale((locale) => formatCombatEvent(locale, event));
  const messageType: MessageEntry["type"] = event.kind === "active_skill" ? "skill" : "combat";
  const { actorSide, targetSide } = sidesForCombatEvent(event);
  queueBattleAnimation(
    resolveCombatTimeline(
      event,
      ++battleAnimationId,
      actorSide,
      targetSide,
      text,
      visualProfileForSide(actorSide),
      visualProfileForSide(targetSide),
      combatStyleForSide(actorSide),
      combatStyleForSide(targetSide)
    ),
    messageType,
    text
  );
}

function handleCombatSettlement([, enemy, won]: [string, string, boolean]) {
  const text = withLocale((locale) => t(locale, won ? "message.combat.victory" : "message.combat.defeat", { enemy }));
  const actorSide: BattleSide = won ? "player" : "enemy";
  const targetSide: BattleSide = actorSide === "player" ? "enemy" : "player";
  queueBattleAnimation(
    resolveSettlementTimeline(
      ++battleAnimationId,
      actorSide,
      targetSide,
      text,
      visualProfileForSide(actorSide),
      visualProfileForSide(targetSide),
      combatStyleForSide(actorSide),
      combatStyleForSide(targetSide)
    ),
    "combat",
    text,
    () => {
      game.update((state) => ({
        ...state,
        battle: {
          ...state.battle,
          active: false,
          enemy: null,
          activeSkills: [],
          cooldowns: {},
          animation: { activeTimeline: null, queueDepth: 0 },
          apSyncedAt: 0,
          actionLockUntil: 0
        }
      }));
      sendAction({ other: "view" });
      sendAction({ other: "quests" });
      sendAction({ other: "inventory" });
      sendAction({ other: "arts" });
    }
  );
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

  game.update((state) => {
    const syncedAt = performance.now();
    const serverLockUntil = syncedAt + Math.max(0, snapshot.battleSnapshotActionLockRemaining || 0) * 1000;
    const existingLockUntil = state.battle.actionLockUntil > syncedAt ? state.battle.actionLockUntil : 0;
    const actionLockUntil = Math.max(serverLockUntil, existingLockUntil);
    return {
      ...state,
      stats: {
        ...state.stats,
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
        activeSkills: snapshot.battleSnapshotActiveSkills || [],
        animation: state.battle.animation,
        apSyncedAt: Math.max(syncedAt, actionLockUntil),
        actionLockUntil
      }
    };
  });
}

function handlePlayerStats(payload: PlayerStatsPayload) {
  game.update((state) => {
    const { stats, status } = normalizePlayerStats(payload, state.stats, state.playerStatus);
    return {
      ...state,
      playerStatus: status,
      stats,
      battle: { ...state.battle, active: status === "in_battle" || state.battle.active }
    };
  });
}

function normalizePlayerStats(payload: PlayerStatsPayload, current: PlayerStats, currentStatus: string) {
  if (Array.isArray(payload)) {
    const [hp, maxHp, qi, maxQi, ap, status] = payload;
    return {
      status,
      stats: { ...current, hp, maxHp, qi, maxQi, ap }
    };
  }

  const status = payload.playerStatsSummaryStatus || currentStatus;
  return {
    status,
    stats: {
      hp: payload.playerStatsSummaryHp ?? current.hp,
      maxHp: payload.playerStatsSummaryMaxHp ?? current.maxHp,
      qi: payload.playerStatsSummaryQi ?? current.qi,
      maxQi: payload.playerStatsSummaryMaxQi ?? current.maxQi,
      jing: payload.playerStatsSummaryJing ?? current.jing,
      maxJing: payload.playerStatsSummaryMaxJing ?? current.maxJing,
      ap: payload.playerStatsSummaryAp ?? current.ap,
      gender: payload.playerStatsSummaryGender || current.gender,
      appearance: payload.playerStatsSummaryAppearance ?? current.appearance,
      appearanceText: payload.playerStatsSummaryAppearanceText || current.appearanceText,
      portraitKey: payload.playerStatsSummaryPortraitKey || current.portraitKey,
      strength: payload.playerStatsSummaryStrength ?? current.strength,
      agility: payload.playerStatsSummaryAgility ?? current.agility,
      vitality: payload.playerStatsSummaryVitality ?? current.vitality
    }
  };
}

function queueBattleAnimation(timeline: ResolvedBattleTimeline, messageType: MessageEntry["type"], messageText: string, after?: () => void) {
  battleTimelineQueue.push({ timeline, messageType, messageText, after });
  if (!latestState.battle.animation.activeTimeline && battleAnimationTimer === null) {
    playNextBattleTimeline();
  } else {
    refreshBattleQueueDepth();
  }
}

function playNextBattleTimeline() {
  const next = battleTimelineQueue.shift();
  if (!next) {
    game.update((state) => ({
      ...state,
      battle: {
        ...state.battle,
        animation: { activeTimeline: null, queueDepth: 0 }
      }
    }));
    return;
  }

  addMessage(next.messageType, next.messageText);
  game.update((state) => ({
    ...state,
    battle: {
      ...state.battle,
      active: true,
      actionLockUntil: Math.max(state.battle.actionLockUntil, performance.now() + next.timeline.durationMs),
      animation: {
        activeTimeline: next.timeline,
        queueDepth: battleTimelineQueue.length + 1
      }
    }
  }));

  battleAnimationTimer = window.setTimeout(() => {
    battleAnimationTimer = null;
    next.after?.();
    if (next.after) {
      battleTimelineQueue.length = 0;
      return;
    }
    game.update((state) => ({
      ...state,
      battle: {
        ...state.battle,
        animation: {
          activeTimeline: null,
          queueDepth: battleTimelineQueue.length
        }
      }
    }));
    playNextBattleTimeline();
  }, next.timeline.durationMs);
}

function refreshBattleQueueDepth() {
  game.update((state) => ({
    ...state,
    battle: {
      ...state.battle,
      animation: {
        ...state.battle.animation,
        queueDepth: battleTimelineQueue.length + (state.battle.animation.activeTimeline ? 1 : 0)
      }
    }
  }));
}

function sidesForCombatEvent(event: CombatEvent): { actorSide: BattleSide; targetSide: BattleSide } {
  if (event.kind === "effect_tick") {
    const targetSide = sideForCombatant(event.targetName, "enemy");
    return {
      actorSide: targetSide === "player" ? "enemy" : "player",
      targetSide
    };
  }

  const actorSide = sideForCombatant(event.actorName, "player");
  const targetSide = sideForCombatant(event.targetName, actorSide === "player" ? "enemy" : "player");
  return { actorSide, targetSide };
}

function sideForCombatant(name: string, fallback: BattleSide = "enemy"): BattleSide {
  const battle = latestState.battle;
  if (name && (name === battle.player?.combatantSnapshotName || name === latestState.username)) return "player";
  if (name && name === battle.enemy?.combatantSnapshotName) return "enemy";
  return fallback;
}

function visualProfileForSide(side: BattleSide) {
  const battle = latestState.battle;
  const gender =
    side === "player" ? battle.player?.combatantSnapshotGender || latestState.stats.gender : battle.enemy?.combatantSnapshotGender;
  return visualProfileFromGender(gender);
}

function combatStyleForSide(side: BattleSide) {
  const battle = latestState.battle;
  const style = side === "player" ? battle.player?.combatantSnapshotCombatStyle : battle.enemy?.combatantSnapshotCombatStyle;
  return combatStyleFromSnapshot(style);
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

function normalizeMapOverview(raw: unknown): MapOverviewSummary | null {
  if (!raw || typeof raw !== "object") return null;

  const obj = raw as Record<string, unknown>;
  const roomsRaw = arrayValue(obj.rooms, obj.mapOverviewSummaryRooms);
  const edgesRaw = arrayValue(obj.edges, obj.mapOverviewSummaryEdges);
  const rooms: MapOverviewSummary["rooms"] = [];
  const edges: MapOverviewSummary["edges"] = [];

  for (const room of roomsRaw) {
    if (!room || typeof room !== "object") continue;
    const roomObj = room as Record<string, unknown>;
    const position = normalizePosition(roomObj.position || roomObj.mapRoomSummaryPosition);
    if (!position) continue;
    rooms.push({
      roomId: typeof roomObj.roomId === "string" ? roomObj.roomId : typeof roomObj.mapRoomSummaryRoomId === "string" ? roomObj.mapRoomSummaryRoomId : null,
      roomName: String(roomObj.roomName || roomObj.mapRoomSummaryRoomName || ""),
      roomKind: String(roomObj.roomKind || roomObj.mapRoomSummaryRoomKind || "building"),
      position
    });
  }

  for (const edge of edgesRaw) {
    if (!edge || typeof edge !== "object") continue;
    const edgeObj = edge as Record<string, unknown>;
    const from = normalizePosition(edgeObj.from || edgeObj.mapEdgeSummaryFromPosition);
    const to = normalizePosition(edgeObj.to || edgeObj.mapEdgeSummaryToPosition);
    if (!from || !to) continue;
    edges.push({
      direction: normalizeDirection(String(edgeObj.direction || edgeObj.mapEdgeSummaryDirection || "")),
      from,
      to,
      toRoomId: typeof edgeObj.toRoomId === "string" ? edgeObj.toRoomId : typeof edgeObj.mapEdgeSummaryToRoomId === "string" ? edgeObj.mapEdgeSummaryToRoomId : null,
      toRoomName: typeof edgeObj.toRoomName === "string" ? edgeObj.toRoomName : typeof edgeObj.mapEdgeSummaryToRoomName === "string" ? edgeObj.mapEdgeSummaryToRoomName : null,
      toMapId: typeof edgeObj.toMapId === "string" ? edgeObj.toMapId : typeof edgeObj.mapEdgeSummaryToMapId === "string" ? edgeObj.mapEdgeSummaryToMapId : null,
      toMapName: typeof edgeObj.toMapName === "string" ? edgeObj.toMapName : typeof edgeObj.mapEdgeSummaryToMapName === "string" ? edgeObj.mapEdgeSummaryToMapName : null
    });
  }

  return {
    mapId: String(obj.mapId || obj.mapOverviewSummaryMapId || ""),
    mapName: String(obj.mapName || obj.mapOverviewSummaryMapName || ""),
    currentPosition: normalizePosition(obj.currentPosition || obj.mapOverviewSummaryCurrentPosition),
    rooms,
    edges
  };
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
        return { direction: normalizeDirection(exit), mapId: null, roomId: null, roomName: null, position: null };
      }
      if (!exit || typeof exit !== "object") return null;
      const obj = exit as Record<string, unknown>;
      return {
        direction: normalizeDirection(String(obj.direction || obj.roomExitSummaryDirection || "")),
        mapId: typeof obj.mapId === "string" ? obj.mapId : typeof obj.roomExitSummaryMapId === "string" ? obj.roomExitSummaryMapId : null,
        roomId: typeof obj.roomId === "string" ? obj.roomId : typeof obj.roomExitSummaryRoomId === "string" ? obj.roomExitSummaryRoomId : null,
        roomName: typeof obj.roomName === "string" ? obj.roomName : typeof obj.roomExitSummaryRoomName === "string" ? obj.roomExitSummaryRoomName : null,
        position: (obj.position || obj.roomExitSummaryPosition || null) as RoomExitSummary["position"]
      };
    })
    .filter((exit): exit is RoomExitSummary => Boolean(exit?.direction));
}

function normalizePosition(raw: unknown): RoomPosition | null {
  if (Array.isArray(raw) && raw.length >= 2) {
    const x = Number(raw[0]);
    const y = Number(raw[1]);
    return Number.isFinite(x) && Number.isFinite(y) ? [x, y] : null;
  }
  if (raw && typeof raw === "object") {
    const position = raw as { x?: unknown; y?: unknown };
    const x = Number(position.x);
    const y = Number(position.y);
    return Number.isFinite(x) && Number.isFinite(y) ? { x, y } : null;
  }
  return null;
}

function arrayValue(...values: unknown[]) {
  for (const value of values) {
    if (Array.isArray(value)) return value;
  }
  return [];
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

function formatCombatEvent(locale: Locale, event: CombatEvent) {
  const action = formatCombatMessage(locale, event.message);
  if (event.kind === "effect_tick") return action;
  if (event.kind === "active_skill") {
    if ((event.damage || 0) > 0) {
      return t(locale, "message.active_skill_damage", {
        caster: event.actorName,
        target: event.targetName,
        action,
        damage: event.damage || 0
      });
    }
    if ((event.heal || 0) > 0) {
      return t(locale, "message.active_skill_heal", {
        caster: event.actorName,
        target: event.targetName,
        action,
        heal: event.heal || 0
      });
    }
    return t(locale, "message.active_skill", { caster: event.actorName, target: event.targetName, action });
  }

  return t(locale, "message.combat.damage", {
    attacker: event.actorName,
    defender: event.targetName,
    action,
    damage: event.damage || 0
  });
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
