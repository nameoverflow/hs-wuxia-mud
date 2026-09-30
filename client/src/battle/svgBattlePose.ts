import { blendPose, svgPose, type SvgPose } from './svgPoseLibrary';
import type { BattleApproach, BattleSide, CombatStyle, PoseKeyDefinition, ResolvedBattleTimeline } from './animationTypes';
import { activeReactionAt, actorOffsetAt, hitForPinnedKey, poseKeyAt, visualTimeAt } from './battleTiming';

export { blendPose, svgPose, type SvgPose };
export const SVG_SWORD_LENGTH = 72;

const clamp = (n: number) => Math.max(0, Math.min(1, n));
const progress = (t: number, a: number, b: number) => clamp((t - a) / Math.max(1, b - a));

/** One compressed silhouette and one launch: no alternating walk cycle. */
export function sampleApproachPose(approach: BattleApproach, prepare: string, style: CombatStyle, phase: number): SvgPose {
  // 身法只有两拍：扎住架势，然后换位。中间不做行走循环。
  if (phase < 0.12) return svgPose('idle', style);
  const p = svgPose(approach.pose, style);
  return phase < 0.86 ? p : blendPose(p, svgPose(prepare, style), clamp((phase - .86) / .14));
}

/**
 * Shares the director's markers and hit stop; no CSS/SMIL animation clock.
 * 姿势之间是硬切，不是插值：蓄势定住、出招一帧到位、余劲再切、收势切回待机。
 */
export function sampleSvgPose(timeline: ResolvedBattleTimeline | null, side: BattleSide, elapsed: number, style: CombatStyle, frame: string, reduced = false): SvgPose {
  const idle = svgPose('idle', style);
  if (!timeline || timeline.kind === 'settlement' || timeline.kind === 'effect_tick') return idle;
  const c = timeline.choreography;
  const arrival = timeline.actor.actionDelayMs;
  if (reduced) return side === timeline.actor.side && elapsed < arrival ? idle : svgPose(frame, style);
  const t = visualTimeAt(timeline, elapsed);
  if (side !== timeline.actor.side) {
    // 闪避提前，招架稍早，受击严格在接触帧之后；都是硬切到位。多段时跟着最近开始反应的那一段。
    const active = activeReactionAt(timeline, t);
    if (!active || active.hit.reaction === 'none' || active.hit.reaction === 'effect') return idle;
    const look = active.hit.staging.reactions[active.hit.reaction];
    const held = svgPose(look.pose ?? active.hit.targetVisual.frames[0].frameId, style);
    // 收势：先切回半个待机，再切干净，避免直接弹回原姿势。
    if (t >= c.restAtMs) return blendPose(idle, held, 0.4);
    return held;
  }
  const keyPoses = timeline.actor.visual.keyPoses;
  if (timeline.actor.motion === 'focus') {
    if (t < c.launchAtMs) return idle;
    return t < c.restAtMs ? svgPose(keyPoses?.contact ?? 'guard', style) : idle;
  }
  const keys = timeline.actor.poseKeys;
  if (!keys.length) return svgPose(frame, style);
  if (t < arrival && timeline.actor.approach) return sampleApproachPose(timeline.actor.approach, keys[0].pose, style, progress(t, 0, arrival));
  // 姿势键之间一律硬切：蓄势定住、出招一帧到位并随定格持住、余劲另起、收势切回待机。
  return keyedPose(poseKeyAt(keys, t) ?? keys[0], timeline, style, t);
}

/** 接触键把拳、脚或剑尖钉在 reach/contactY 上。招架时留出一段距离，让兵刃停在格挡位置而不是穿进身体。 */
function keyedPose(key: PoseKeyDefinition, timeline: ResolvedBattleTimeline, style: CombatStyle, t: number): SvgPose {
  const pose = svgPose(key.pose, style);
  if (!key.pin) return pose;
  const c = timeline.choreography;
  const hit = hitForPinnedKey(timeline, key);
  // 身法轨道挪动了根节点时，接触点要反向补回，拳脚才会仍然落在对手身上。
  const offset = actorOffsetAt(timeline, t);
  const reach = (key.reach ?? c.reach) - (hit.result === 'parry' ? hit.staging.reactions.parry.standoff ?? 0 : 0) - offset.x;
  const y = (key.contactY ?? c.contactY) - 176 - offset.y;
  if (key.pin === 'foot') pose.foot = [reach, y];
  else if (key.pin === 'hand') pose.hand = [reach, y];
  else {
    const radians = pose.blade * Math.PI / 180;
    pose.hand = [reach - Math.cos(radians) * SVG_SWORD_LENGTH, y - Math.sin(radians) * SVG_SWORD_LENGTH];
    pose.elbow = [(pose.shoulder[0] + pose.hand[0]) / 2, pose.hand[1] + 10];
  }
  return pose;
}
