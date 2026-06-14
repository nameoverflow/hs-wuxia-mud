import type { CombatEvent, CombatResult, CombatVisualHint } from "../protocol";
import {
  actionPools,
  actorIdleSprite,
  battleActions,
  reactionSprites,
  spriteForClip
} from "./animationCatalog";
import type { BattleActionDefinition, BattleSide, ResolvedBattleTimeline, TargetReaction, TimelineVfx } from "./animationTypes";

const defaultAction = battleActions["sword.slash_a"];

export function resolveCombatTimeline(
  event: CombatEvent,
  id: number,
  actorSide: BattleSide,
  targetSide: BattleSide,
  text: string
): ResolvedBattleTimeline {
  const action = selectAction(event.visual);
  const result = event.result || "hit";
  const reaction = action.targetReaction[result] || resultReaction(result);
  const targetSprite = reaction === "effect" || reaction === "none" ? actorIdleSprite : reactionSprites[reaction] || actorIdleSprite;

  return {
    id,
    kind: event.kind,
    actorSide,
    targetSide,
    durationMs: action.durationMs,
    actor: {
      side: actorSide,
      sprite: spriteForClip(action.clipId),
      motion: action.actorMotion
    },
    target: {
      side: targetSide,
      sprite: targetSprite,
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

export function resolveSettlementTimeline(id: number, actorSide: BattleSide, targetSide: BattleSide, text: string): ResolvedBattleTimeline {
  return {
    id,
    kind: "settlement",
    actorSide,
    targetSide,
    durationMs: 900,
    actor: {
      side: actorSide,
      sprite: actorIdleSprite,
      motion: "none"
    },
    target: {
      side: targetSide,
      sprite: actorIdleSprite,
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

function selectAction(visual: CombatVisualHint | null | undefined): BattleActionDefinition {
  if (visual?.action && battleActions[visual.action]) {
    return battleActions[visual.action];
  }

  const poolIds = (visual?.pool && actionPools[visual.pool]) || actionPools["weapon.sword.basic"];
  const candidates = poolIds.map((id) => battleActions[id]).filter(Boolean);
  if (!candidates.length) return defaultAction;

  const requestedTags = new Set((visual?.tags || []).map((tag) => tag.toLowerCase()));
  const tagged = candidates.filter((action) => action.tags.some((tag) => requestedTags.has(tag)));
  const pool = tagged.length ? tagged : candidates;
  return pool[Math.floor(Math.random() * pool.length)] || defaultAction;
}

function resolveVfx(action: BattleActionDefinition, actorSide: BattleSide, targetSide: BattleSide, result: CombatResult): TimelineVfx[] {
  return action.vfx
    .filter((vfx) => {
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
