import type { CombatEvent, CombatResult, CombatVisualHint } from "../protocol";
import {
  actionPools,
  actionVariants,
  battleActions,
  idleVisualForStyle,
  reactionVisualFor,
  visualForClip
} from "./animationCatalog";
import type {
  ActionPoolEntry,
  ActionPoolVariant,
  ActorVisual,
  BattleActionDefinition,
  BattleSide,
  CombatStyle,
  ResolvedBattleTimeline,
  TargetReaction,
  TimelineVfx,
  VisualProfile
} from "./animationTypes";

const defaultActions: Record<CombatStyle, BattleActionDefinition> = {
  sword: battleActions["sword.slash_a"],
  fist: battleActions["fist.punch"]
};

const defaultPools: Record<CombatStyle, string> = {
  sword: "weapon.sword.basic",
  fist: "weapon.fist.basic"
};

interface ResolvedActionCandidate {
  action: BattleActionDefinition;
  weight: number;
  tags: string[];
}

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
  const action = selectAction(event.visual, actorProfile, actorStyle);
  const result = event.result || "hit";
  const reaction = action.targetReaction[result] || resultReaction(result);
  const actorVisual = visualForClip(action.clipId, actorProfile, actorStyle);
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
      sprite: spriteFromVisual(actorVisual),
      visual: actorVisual,
      motion: action.actorMotion
    },
    target: {
      side: targetSide,
      sprite: spriteFromVisual(targetVisual),
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
      sprite: spriteFromVisual(actorVisual),
      visual: actorVisual,
      motion: "none"
    },
    target: {
      side: targetSide,
      sprite: spriteFromVisual(targetVisual),
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

function spriteFromVisual(visual: ActorVisual) {
  return visual.sprite;
}

function selectAction(
  visual: CombatVisualHint | null | undefined,
  profile: VisualProfile,
  fallbackStyle: CombatStyle
): BattleActionDefinition {
  const requestedAction = visual?.action;
  const explicitAction = resolveActionVariant(requestedAction, profile, fallbackStyle);
  if (explicitAction && battleActions[explicitAction]) {
    return battleActions[explicitAction];
  }
  if (requestedAction && battleActions[requestedAction]) {
    return battleActions[requestedAction];
  }

  const defaultAction = defaultActions[fallbackStyle];
  const candidates = resolveActionPool(visual?.pool || defaultPools[fallbackStyle], profile, fallbackStyle);
  if (!candidates.length) return defaultAction;

  const requestedTags = new Set((visual?.tags || []).map((tag) => tag.toLowerCase()));
  const scored = candidates.map((candidate) => ({
    ...candidate,
    score: candidate.tags.filter((tag) => requestedTags.has(tag)).length
  }));
  const bestScore = Math.max(...scored.map(({ score }) => score));
  const tagged = bestScore > 0 ? scored.filter(({ score }) => score === bestScore) : [];
  const pool = tagged.length ? tagged : candidates;
  return weightedPick(pool)?.action || defaultAction;
}

function resolveActionPool(
  poolId: string | null | undefined,
  profile: VisualProfile,
  fallbackStyle: CombatStyle,
  seen: Set<string> = new Set()
): ResolvedActionCandidate[] {
  const resolvedPoolId = poolId || defaultPools[fallbackStyle];
  const pool = actionPools[resolvedPoolId];
  if (!pool) {
    return resolvedPoolId === defaultPools[fallbackStyle] ? [] : resolveActionPool(defaultPools[fallbackStyle], profile, fallbackStyle, seen);
  }
  if (seen.has(resolvedPoolId)) return [];
  seen.add(resolvedPoolId);

  const entries = applyPoolVariant(
    applyPoolVariant(applyPoolVariant(pool.actions, pool.styles?.[fallbackStyle]), pool.profiles?.[profile]),
    pool.styleProfiles?.[fallbackStyle]?.[profile]
  );
  const candidates = entries
    .map((entry) => resolveActionCandidate(entry, profile, fallbackStyle))
    .filter((candidate): candidate is ResolvedActionCandidate => Boolean(candidate));

  if (candidates.length) return candidates;
  if (pool.fallbackPool) return resolveActionPool(pool.fallbackPool, profile, fallbackStyle, seen);
  return resolvedPoolId === defaultPools[fallbackStyle] ? [] : resolveActionPool(defaultPools[fallbackStyle], profile, fallbackStyle, seen);
}

function applyPoolVariant(entries: ActionPoolEntry[], variant: ActionPoolVariant | undefined): ActionPoolEntry[] {
  if (!variant) return entries;
  return variant.mode === "replace" ? variant.actions : [...entries, ...variant.actions];
}

function resolveActionCandidate(
  entry: ActionPoolEntry,
  profile: VisualProfile,
  fallbackStyle: CombatStyle
): ResolvedActionCandidate | null {
  const rawId = typeof entry === "string" ? entry : entry.id;
  const actionId = resolveActionVariant(rawId, profile, fallbackStyle) || rawId;
  const action = battleActions[actionId] || battleActions[rawId];
  if (!action) return null;
  const extraTags = typeof entry === "string" ? [] : entry.tags || [];
  return {
    action,
    weight: typeof entry === "string" ? 1 : Math.max(0, entry.weight ?? 1),
    tags: [...action.tags, ...extraTags].map((tag) => tag.toLowerCase())
  };
}

function resolveActionVariant(actionId: string | null | undefined, profile: VisualProfile, fallbackStyle: CombatStyle): string | null {
  if (!actionId) return null;
  const variant = actionVariants[actionId];
  return variant?.styleProfiles?.[fallbackStyle]?.[profile] || variant?.profiles?.[profile] || variant?.styles?.[fallbackStyle] || actionId;
}

function weightedPick(candidates: ResolvedActionCandidate[]): ResolvedActionCandidate | null {
  const total = candidates.reduce((sum, candidate) => sum + candidate.weight, 0);
  if (total <= 0) return candidates[0] || null;
  let cursor = Math.random() * total;
  for (const candidate of candidates) {
    cursor -= candidate.weight;
    if (cursor <= 0) return candidate;
  }
  return candidates[candidates.length - 1] || null;
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
