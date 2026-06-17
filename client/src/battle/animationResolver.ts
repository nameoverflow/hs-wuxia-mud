import type { CombatEvent, CombatResult } from "../protocol";
import { idleVisualForStyle, reactionVisualFor, rigActionFor, visualForRigAction } from "./rigActionCatalog";
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
  const action = rigActionFor(event.visual?.actionId, actorStyle);
  const result = event.result || "hit";
  const reaction = action.targetReaction[result] || resultReaction(result);
  const actorVisual = visualForRigAction(action.id, actorProfile, actorStyle);
  const targetVisual =
    reaction === "effect" || reaction === "none" ? idleVisualForStyle(targetStyle, targetProfile) : reactionVisualFor(reaction, targetProfile, targetStyle);

  return {
    id,
    kind: event.kind,
    actorSide,
    targetSide,
    durationMs: action.durationMs,
    actor: {
      side: actorSide,
      visual: actorVisual,
      motion: action.actorMotion
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
    actor: {
      side: actorSide,
      visual: actorVisual,
      motion: "none"
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
