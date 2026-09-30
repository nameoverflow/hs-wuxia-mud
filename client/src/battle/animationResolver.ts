import type { CombatEvent, CombatResult } from "../protocol";
import { clipImpactOffsetMs } from "./animationClip";
import { battleActionFor, idleVisualForStyle, reactionVisualFor, visualForBattleAction } from "./battleActionCatalog";
import type {
  ActionVfxDefinition,
  BattleActionDefinition,
  BattleSide,
  CombatStyle,
  ResolvedBattleTimeline,
  TargetReaction,
  TimelineVfx,
  VisualProfile
} from "./animationTypes";

export function resolveCombatTimeline(
  event: CombatEvent,
  id: number,
  actorSide: BattleSide,
  targetSide: BattleSide,
  text: string,
  actorProfile: VisualProfile = "male",
  targetProfile: VisualProfile = "male",
  actorStyle: CombatStyle = "fist",
  targetStyle: CombatStyle = "fist"
): ResolvedBattleTimeline {
  const action = battleActionFor(event.visual?.actionId, actorStyle);
  const result = event.result || "hit";
  const reaction = action.targetReaction[result] || resultReaction(result);
  const actorVisual = visualForBattleAction(action.id, actorProfile, actorStyle);
  const actionDurationMs = visualDurationMs(event.visual?.durationMs, action.durationMs);
  const approach = event.kind === 'effect_tick' ? undefined : action.approach;
  const actionDelayMs = (approach?.durationMs ?? 0) * actionDurationMs / action.durationMs;
  const impactAtMs = actionDelayMs + clipImpactOffsetMs(action.frames, action.impactFrame, actionDurationMs);
  const targetVisual =
    reaction === "effect" || reaction === "none" ? idleVisualForStyle(targetStyle, targetProfile) : reactionVisualFor(reaction, targetProfile, targetStyle);

  return {
    id,
    kind: event.kind,
    actorSide,
    targetSide,
    durationMs: actionDurationMs + actionDelayMs,
    impactAtMs,
    label: event.message?.kind === "script" && event.message.text.trim().length <= 12 ? event.message.text.trim() : action.label,
    choreography: {
      ...action.choreography,
      launchAtMs: actionDelayMs + action.choreography.launchAtMs * actionDurationMs / action.durationMs,
      hitStopMs: action.choreography.hitStopMs * actionDurationMs / action.durationMs,
      recoverAtMs: actionDelayMs + action.choreography.recoverAtMs * actionDurationMs / action.durationMs,
      restAtMs: actionDelayMs + action.choreography.restAtMs * actionDurationMs / action.durationMs
    },
    actor: {
      side: actorSide,
      visual: actorVisual,
      motion: action.actorMotion,
      approach,
      actionDelayMs
    },
    target: {
      side: targetSide,
      visual: targetVisual,
      reaction
    },
    result,
    damage: event.damage,
    heal: event.heal,
    floatText: floatText(event),
    text,
    vfx: resolveVfx(action, actorSide, targetSide, result)
  };
}

export function resolveSettlementTimeline(
  id: number,
  actorSide: BattleSide,
  targetSide: BattleSide,
  text: string,
  actorProfile: VisualProfile = "male",
  targetProfile: VisualProfile = "male",
  actorStyle: CombatStyle = "fist",
  targetStyle: CombatStyle = "fist"
): ResolvedBattleTimeline {
  const actorVisual = idleVisualForStyle(actorStyle, actorProfile);
  const targetVisual = idleVisualForStyle(targetStyle, targetProfile);

  return {
    id,
    kind: "settlement",
    actorSide,
    targetSide,
    durationMs: 900,
    impactAtMs: 0,
    label: actorSide === "player" ? "胜" : "败",
    choreography: { launchAtMs: 0, hitStopMs: 0, recoverAtMs: 0, restAtMs: 900, reach: 0, contactY: 100, weight: "quiet" },
    actor: {
      side: actorSide,
      visual: actorVisual,
      motion: "none",
      actionDelayMs: 0
    },
    target: {
      side: targetSide,
      visual: targetVisual,
      reaction: "none"
    },
    result: "effect",
    damage: null,
    heal: null,
    floatText: "",
    text,
    vfx: []
  };
}

function visualDurationMs(serverDurationMs: number | null | undefined, fallbackDurationMs: number) {
  return typeof serverDurationMs === "number" && Number.isFinite(serverDurationMs) && serverDurationMs > 0 ? Math.round(serverDurationMs) : fallbackDurationMs;
}

function resolveVfx(action: BattleActionDefinition, actorSide: BattleSide, targetSide: BattleSide, result: CombatResult): TimelineVfx[] {
  return action.vfx
    .filter((vfx: ActionVfxDefinition) => {
      if (vfx.kind === "impact" && result !== "hit") return false;
      if (vfx.kind === "parry" && result !== "parry") return false;
      return true;
    })
    .map((vfx, index) => ({
      id: `${action.id}-${vfx.kind}-${vfx.variant}-${index}`,
      kind: vfx.kind,
      variant: vfx.variant,
      art: vfx.art,
      side: vfx.anchor === "actor" ? actorSide : vfx.anchor === "target" ? targetSide : "center"
    }));
}

function resultReaction(result: CombatResult): TargetReaction {
  if (result === "hit") return "hit";
  if (result === "dodge") return "dodge";
  if (result === "parry") return "parry";
  return "effect";
}

function floatText(event: CombatEvent) {
  if ((event.damage || 0) > 0) return `-${event.damage}`;
  if ((event.heal || 0) > 0) return `+${event.heal}`;
  if (event.result === "dodge") return "闪";
  if (event.result === "parry") return "架";
  return "";
}
