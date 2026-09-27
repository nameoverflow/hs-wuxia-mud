import { get } from "svelte/store";
import { game, clearBattleAnimationQueue, processServerMessage } from "../game";
import { battleActionFor } from "./battleActionCatalog";
import type { BattleSnapshot, CombatEvent, CombatResult, CombatantSnapshot } from "../protocol";
import type { BattleSide, CombatStyle, VisualProfile } from "./animationTypes";

function combatant(name: string, style: CombatStyle, gender: VisualProfile): CombatantSnapshot {
  return { combatantSnapshotId: name, combatantSnapshotName: name, combatantSnapshotGender: gender, combatantSnapshotCombatStyle: style,
    combatantSnapshotHp: 180, combatantSnapshotMaxHp: 180, combatantSnapshotQi: 100, combatantSnapshotMaxQi: 100,
    combatantSnapshotAp: 100, combatantSnapshotAgility: 18, combatantSnapshotEffects: [] };
}

export function seedBattle(playerProfile: VisualProfile = "female", enemyProfile: VisualProfile = "male") {
  clearBattleAnimationQueue();
  game.update((state) => ({ ...state, connected: true, username: "行者", stats: { ...state.stats, gender: playerProfile },
    room: { ...state.room, name: "竹间试招" }, messages: [], battle: { ...state.battle, presentation: undefined } }));
  const snapshot: BattleSnapshot = { battleSnapshotPlayer: combatant("行者", "fist", playerProfile), battleSnapshotEnemy: combatant("守擂人", "sword", enemyProfile),
    battleSnapshotActiveSkillCooldowns: [], battleSnapshotActiveSkills: [], battleSnapshotActionLockRemaining: 0 };
  processServerMessage({ tag: "BattleStateMsg", contents: snapshot });
}

export function submitBattleAction(actionId: string, result: CombatResult = "hit", actorSide: BattleSide = "player", damage = 18) {
  const state = get(game);
  const action = battleActionFor(actionId, "fist");
  const player = { ...state.battle.player! };
  const enemy = { ...state.battle.enemy! };
  const actor = actorSide === "player" ? player : enemy;
  const self = action.actorMotion === "focus";
  const target = self ? actor : actorSide === "player" ? enemy : player;
  if (!actionId.startsWith("rig.effect.")) actor.combatantSnapshotCombatStyle = action.style;
  const snapshot = (): BattleSnapshot => ({ battleSnapshotPlayer: { ...player }, battleSnapshotEnemy: { ...enemy }, battleSnapshotActiveSkillCooldowns: [], battleSnapshotActiveSkills: [], battleSnapshotActionLockRemaining: action.durationMs / 1000 });
  processServerMessage({ tag: "BattleStateMsg", contents: snapshot() });
  const heal = actionId.includes("healing") || actionId.endsWith("hot") ? 22 : null;
  const event: CombatEvent = { kind: actionId.startsWith("rig.effect.") ? "effect_tick" : self ? "active_skill" : "normal",
    actorName: actor.combatantSnapshotName, targetName: target.combatantSnapshotName,
    message: { kind: "script", text: action.label }, damage: result === "hit" && !heal ? damage : null, heal,
    result: self || heal ? "effect" : result, visual: { actionId, durationMs: action.durationMs, tags: action.tags } };
  processServerMessage({ tag: "CombatEventMsg", contents: event });
  target.combatantSnapshotHp = Math.max(0, Math.min(target.combatantSnapshotMaxHp, target.combatantSnapshotHp - (event.damage || 0) + (event.heal || 0)));
  processServerMessage({ tag: "BattleStateMsg", contents: snapshot() });
}

export function playBattleDemo() {
  seedBattle();
  submitBattleAction("rig.fist.punch_a", "hit");
  submitBattleAction("rig.fist.punch_a", "hit");
  submitBattleAction("rig.sword.thrust_a", "parry", "enemy");
  submitBattleAction("rig.fist.kick_a", "dodge");
  submitBattleAction("rig.sword.cut_a", "hit", "enemy", 26);
  submitBattleAction("rig.fist.heavy_a", "hit", "player", 35);
  submitBattleAction("rig.fist.healing_palm", "effect");
  submitBattleAction("rig.sword.rising_cut_a", "dodge", "enemy");
  submitBattleAction("rig.fist.heavy_a", "hit", "player", 109);
  processServerMessage({ tag: "CombatSettlementMsg", contents: ["行者", "守擂人", true] });
}
