import type { CombatEvent, CombatResult } from "../protocol";
import { clipImpactOffsetMs } from "./animationClip";
import { battleActionFor, idleVisualForStyle, reactionVisualFor, visualForBattleAction } from "./battleActionCatalog";
import type {
  ActionVfxDefinition,
  BattleActionDefinition,
  BattleSide,
  CombatStyle,
  PoseKeyDefinition,
  ResolvedBattleTimeline,
  ResolvedHit,
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
  const scale = actionDurationMs / action.durationMs;
  const hits = resolveHits(action, event, actionDelayMs, actionDurationMs);
  const choreography = {
    ...action.choreography,
    launchAtMs: actionDelayMs + action.choreography.launchAtMs * scale,
    hitStopMs: hits[0].hitStopMs,
    recoverAtMs: actionDelayMs + action.choreography.recoverAtMs * scale,
    restAtMs: actionDelayMs + action.choreography.restAtMs * scale
  };
  const targetVisual =
    reaction === "effect" || reaction === "none" ? idleVisualForStyle(targetStyle, targetProfile) : reactionVisualFor(reaction, targetProfile, targetStyle);

  return {
    id,
    kind: event.kind,
    actorSide,
    targetSide,
    durationMs: actionDurationMs + actionDelayMs,
    impactAtMs: hits[0].atMs,
    hits,
    label: event.message?.kind === "script" && event.message.text.trim().length <= 12 ? event.message.text.trim() : action.label,
    choreography,
    actor: {
      side: actorSide,
      visual: actorVisual,
      motion: action.actorMotion,
      approach,
      poseKeys: resolvePoseKeys(action, choreography, hits, actionDelayMs, scale),
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
    hits: [{ atMs: 0, hitStopMs: 0, damage: null, heal: null, floatText: "" }],
    label: actorSide === "player" ? "胜" : "败",
    choreography: { launchAtMs: 0, hitStopMs: 0, recoverAtMs: 0, restAtMs: 900, reach: 0, contactY: 100, weight: "quiet" },
    actor: {
      side: actorSide,
      visual: actorVisual,
      motion: "none",
      poseKeys: [],
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

function resolveHits(action: BattleActionDefinition, event: CombatEvent, delayMs: number, playbackMs: number): ResolvedHit[] {
  const scale = playbackMs / action.durationMs;
  const source = action.hits?.length
    ? action.hits.map((hit) => ({ atMs: delayMs + Math.round(hit.atMs * scale), hitStopMs: hit.hitStopMs * scale, share: hit.share ?? 1 }))
    : [{ atMs: delayMs + clipImpactOffsetMs(action.frames, action.impactFrame, playbackMs), hitStopMs: action.choreography.hitStopMs * scale, share: 1 }];
  const damage = splitByShare(event.damage, source.map((hit) => hit.share));
  const heal = splitByShare(event.heal, source.map((hit) => hit.share));
  return source.map((hit, index) => ({
    atMs: hit.atMs,
    hitStopMs: hit.hitStopMs,
    damage: damage[index],
    heal: heal[index],
    floatText: floatText({ ...event, damage: damage[index], heal: heal[index] })
  }));
}

/** 按份额拆分总量，累计取整，保证各段之和等于服务端给的总数。 */
function splitByShare(total: number | null | undefined, shares: number[]): (number | null)[] {
  if (total === null || total === undefined) return shares.map(() => null);
  const sum = shares.reduce((a, b) => a + b, 0);
  let before = 0;
  let acc = 0;
  return shares.map((share) => {
    acc += share;
    const upTo = Math.round(total * acc / sum);
    const part = upTo - before;
    before = upTo;
    return part;
  });
}

/**
 * 没有写 poseTrack 的动作，按 keyPoses 展开成默认的四个键：
 * 到位蓄势 → 起手即接触 → 最后一段定格结束切余劲 → 收势待机。
 */
function resolvePoseKeys(
  action: BattleActionDefinition,
  choreography: ResolvedBattleTimeline["choreography"],
  hits: ResolvedHit[],
  delayMs: number,
  scale: number
): PoseKeyDefinition[] {
  if (action.actorMotion === "focus") return [];
  if (action.poseTrack?.length) return action.poseTrack.map((key) => ({ ...key, atMs: delayMs + key.atMs * scale }));
  const keyPoses = action.keyPoses;
  if (!keyPoses?.prepare || !keyPoses.finish || !keyPoses.reachWith) return [];
  const final = hits[hits.length - 1];
  return [
    { atMs: delayMs, pose: keyPoses.prepare },
    { atMs: choreography.launchAtMs, pose: keyPoses.contact, pin: keyPoses.reachWith },
    { atMs: final.atMs + final.hitStopMs, pose: keyPoses.finish },
    { atMs: choreography.restAtMs, pose: "idle" }
  ];
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

function floatText(event: Pick<CombatEvent, "damage" | "heal" | "result">) {
  if ((event.damage || 0) > 0) return `-${event.damage}`;
  if ((event.heal || 0) > 0) return `+${event.heal}`;
  if (event.result === "dodge") return "闪";
  if (event.result === "parry") return "架";
  return "";
}
