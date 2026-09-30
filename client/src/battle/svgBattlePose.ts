import { blendPose, svgPose, type SvgPose } from './svgPoseLibrary';
import type { BattleApproach, BattleSide, CombatStyle, ResolvedBattleTimeline } from './animationTypes';

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
  const impact = timeline.impactAtMs;
  const holdEnd = impact + c.hitStopMs;
  const t = elapsed >= impact && elapsed < holdEnd ? impact : elapsed;
  if (side !== timeline.actor.side) {
    const reaction = timeline.target.reaction;
    if (reaction === 'none' || reaction === 'effect') return idle;
    // 闪避提前，招架稍早，受击严格在接触帧之后；都是硬切到位。
    const at = reaction === 'dodge' ? Math.max(c.launchAtMs, impact - 95) : reaction === 'parry' ? Math.max(c.launchAtMs, impact - 50) : impact;
    if (t < at) return idle;
    const held = svgPose(timeline.target.visual.frames[0].frameId, style);
    // 收势：先切回半个待机，再切干净，避免直接弹回原姿势。
    if (t >= c.restAtMs) return blendPose(idle, held, 0.4);
    return held;
  }
  const keyPoses = timeline.actor.visual.keyPoses;
  if (timeline.actor.motion === 'focus') {
    if (t < c.launchAtMs) return idle;
    return t < c.restAtMs ? svgPose(keyPoses?.contact ?? 'guard', style) : idle;
  }
  if (!keyPoses?.prepare || !keyPoses.finish || !keyPoses.reachWith) return svgPose(frame, style);
  if (t < arrival && timeline.actor.approach) return sampleApproachPose(timeline.actor.approach, keyPoses.prepare, style, progress(t, 0, arrival));
  const prep = svgPose(keyPoses.prepare, style);
  const strike = svgPose(keyPoses.contact, style);
  // 接触点。招架时留出一段距离，让剑停在格挡位置而不是穿进身体。
  const reach = timeline.result === 'parry' ? c.reach - 26 : c.reach;
  if (keyPoses.reachWith === 'foot') strike.foot = [reach, c.contactY - 176];
  else if (keyPoses.reachWith === 'hand') strike.hand = [reach, c.contactY - 176];
  else {
    const radians = strike.blade * Math.PI / 180;
    strike.hand = [reach - Math.cos(radians) * SVG_SWORD_LENGTH, c.contactY - 176 - Math.sin(radians) * SVG_SWORD_LENGTH];
    strike.elbow = [(strike.shoulder[0] + strike.hand[0]) / 2, strike.hand[1] + 10];
  }
  if (t < c.launchAtMs) return prep;
  // 出招一帧到位，并连同命中定格一起持住，定格结束才切到收招姿势。
  if (t < impact + c.hitStopMs) return strike;
  // 余劲：过接触帧后另起一个收招姿势，不回放释放动作。
  if (t < c.restAtMs) return svgPose(keyPoses.finish, style);
  return idle;
}
