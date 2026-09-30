import type { CombatEvent, CombatResult, CombatVisualParams } from "../protocol";
import { stageArt } from "./stageAssets";
import { clipImpactOffsetMs } from "./animationClip";
import { stagingFor } from "./stagingProfile";
import { battleActionFor, idleVisualForStyle, reactionVisualFor, visualForBattleAction } from "./battleActionCatalog";
import type {
  ActionVfxDefinition,
  ActorVisual,
  BattleActionDefinition,
  BattleSide,
  CombatStyle,
  PoseKeyDefinition,
  ResolvedBattleTimeline,
  ResolvedHit,
  StagingOverride,
  VfxArt,
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
  const actorVisual = visualForBattleAction(action.id, actorProfile, actorStyle);
  const actionDurationMs = visualDurationMs(event.visual?.durationMs, action.durationMs);
  const approach = event.kind === 'effect_tick' ? undefined : action.approach;
  const actionDelayMs = (approach?.durationMs ?? 0) * actionDurationMs / action.durationMs;
  const scale = actionDurationMs / action.durationMs;
  const reactionVisual = (reaction: TargetReaction) =>
    reaction === "effect" || reaction === "none" ? idleVisualForStyle(targetStyle, targetProfile) : reactionVisualFor(reaction, targetProfile, targetStyle);
  const params = visualParams(event.visual?.params);
  const hits = resolveHits(action, event, actionDelayMs, actionDurationMs, reactionVisual, params.staging);
  const { result, reaction } = hits[0];
  const choreography = {
    ...action.choreography,
    launchAtMs: actionDelayMs + action.choreography.launchAtMs * scale,
    hitStopMs: hits[0].hitStopMs,
    recoverAtMs: actionDelayMs + action.choreography.recoverAtMs * scale,
    restAtMs: actionDelayMs + action.choreography.restAtMs * scale
  };
  const targetVisual = hits[0].targetVisual;

  return {
    id,
    kind: event.kind,
    actorSide,
    targetSide,
    durationMs: actionDurationMs + actionDelayMs,
    impactAtMs: hits[0].atMs,
    hits,
    staging: stagingFor(action.choreography.weight, action.staging, params.staging),
    label: params.label ?? (event.message?.kind === "script" && event.message.text.trim().length <= 12 ? event.message.text.trim() : action.label),
    choreography,
    actor: {
      side: actorSide,
      visual: actorVisual,
      motion: action.actorMotion,
      approach,
      poseKeys: resolvePoseKeys(action, choreography, hits, actionDelayMs, scale),
      offsetKeys: (action.offsetTrack || []).map((key) => ({ ...key, atMs: actionDelayMs + key.atMs * scale })),
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
    vfx: resolveVfx(action, actorSide, targetSide, result, hits, actionDelayMs, scale)
      .map((vfx) => ({ ...vfx, art: params.vfxArt?.[vfx.kind] ?? vfx.art }))
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
    hits: [{ atMs: 0, hitStopMs: 0, damage: null, heal: null, floatText: "", result: "effect", reaction: "none", targetVisual, staging: stagingFor("quiet") }],
    staging: stagingFor("quiet"),
    label: actorSide === "player" ? "胜" : "败",
    choreography: { launchAtMs: 0, hitStopMs: 0, recoverAtMs: 0, restAtMs: 900, reach: 0, contactY: 100, weight: "quiet" },
    actor: {
      side: actorSide,
      visual: actorVisual,
      motion: "none",
      poseKeys: [],
      offsetKeys: [],
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

function resolveHits(
  action: BattleActionDefinition,
  event: CombatEvent,
  delayMs: number,
  playbackMs: number,
  reactionVisual: (reaction: TargetReaction) => ActorVisual,
  paramStaging?: StagingOverride
): ResolvedHit[] {
  const scale = playbackMs / action.durationMs;
  const source = action.hits?.length
    ? action.hits.map((hit) => ({ atMs: delayMs + Math.round(hit.atMs * scale), hitStopMs: hit.hitStopMs * scale, share: hit.share ?? 1, staging: hit.staging }))
    : [{ atMs: delayMs + clipImpactOffsetMs(action.frames, action.impactFrame, playbackMs), hitStopMs: action.choreography.hitStopMs * scale, share: 1, staging: undefined }];
  // 服务端给了逐段结果就照用；老服务端只有总数时，按份额拆分，各段共用同一个结果。
  const outcomes = event.hits?.length === source.length ? event.hits : undefined;
  const damage = outcomes ? outcomes.map((hit) => hit.damage) : splitByShare(event.damage, source.map((hit) => hit.share));
  const heal = outcomes ? outcomes.map((hit) => hit.heal) : splitByShare(event.heal, source.map((hit) => hit.share));
  return source.map((hit, index) => {
    const result = outcomes?.[index].result || event.result || "hit";
    const reaction = action.targetReaction[result] || resultReaction(result);
    return {
      atMs: hit.atMs,
      hitStopMs: hit.hitStopMs,
      damage: damage[index],
      heal: heal[index],
      floatText: floatText({ result, damage: damage[index], heal: heal[index] }),
      result,
      reaction,
      targetVisual: reactionVisual(reaction),
      staging: stagingFor(action.choreography.weight, action.staging, paramStaging, hit.staging)
    };
  });
}

/** 按份额拆分总量，累计取整，保证各段之和等于服务端给的总数。 */
/** 只取认得的参数；素材名不认识就忽略，避免内容里写错一个字把整场战斗画面弄崩。 */
function visualParams(raw: CombatVisualParams | null | undefined): { label?: string; staging?: StagingOverride; vfxArt?: Partial<Record<string, VfxArt>> } {
  if (!raw || typeof raw !== "object") return {};
  const vfxArt = raw.vfxArt && typeof raw.vfxArt === "object"
    ? Object.fromEntries(Object.entries(raw.vfxArt).filter((entry): entry is [string, VfxArt] => entry[1] in stageArt))
    : undefined;
  return {
    label: typeof raw.label === "string" && raw.label.trim() ? raw.label.trim() : undefined,
    staging: raw.staging && typeof raw.staging === "object" ? (raw.staging as StagingOverride) : undefined,
    vfxArt
  };
}

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

function resolveVfx(
  action: BattleActionDefinition,
  actorSide: BattleSide,
  targetSide: BattleSide,
  result: CombatResult,
  hits: ResolvedHit[],
  delayMs: number,
  scale: number
): TimelineVfx[] {
  return action.vfx
    .filter((vfx: ActionVfxDefinition) => {
      if (vfx.kind === "impact" && result !== "hit") return false;
      if (vfx.kind === "parry" && result !== "parry") return false;
      return true;
    })
    .map((vfx, index) => {
      // sprite/custom 的起止时间：写 atMs 就按动作时钟，否则挂在某一段命中上。
      const anchorHit = hits[Math.min(vfx.hit ?? 0, hits.length - 1)];
      const startMs = vfx.atMs !== undefined ? delayMs + vfx.atMs * scale : anchorHit.atMs + (vfx.offsetMs ?? 0) * scale;
      return {
        ...vfx,
        id: `${action.id}-${vfx.kind}-${vfx.variant}-${index}`,
        kind: vfx.kind,
        variant: vfx.variant,
        art: vfx.art,
        side: vfx.anchor === "actor" ? actorSide : vfx.anchor === "target" ? targetSide : "center",
        startMs,
        endMs: startMs + (vfx.durationMs ?? 200) * scale
      };
    });
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
