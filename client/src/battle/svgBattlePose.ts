import { approachForAction } from "./battleApproach";
import type { BattleSide, CombatStyle, ResolvedBattleTimeline } from './animationTypes';

export const SVG_SWORD_LENGTH = 72;

type Point = [number, number];
export interface SvgPose {
  head: Point; shoulder: Point; hip: Point;
  backElbow: Point; backHand: Point; elbow: Point; hand: Point;
  backKnee: Point; backFoot: Point; knee: Point; foot: Point;
  /** 衣袂末梢。纯装饰的一笔墨线，不参与物理，只随关键姿势定住。 */
  robe: Point;
  /** 剑穗末梢。拳招不使用。 */
  tassel: Point;
  blade: number;
}
export const svgFrameIds = new Set([
  'idle', 'sword_ready', 'punch_windup', 'punch_strike', 'kick_windup', 'kick_strike',
  'heavy_windup', 'heavy_strike', 'guard', 'hurt', 'dodge', 'sword_hurt', 'sword_dodge', 'sword_parry',
  ...['thrust', 'cut', 'chop', 'rising'].flatMap(action => ['prep', 'strike', 'close'].map(phase => `${action}_${phase}`))
]);
const clamp = (n: number) => Math.max(0, Math.min(1, n));
const progress = (t: number, a: number, b: number) => clamp((t - a) / Math.max(1, b - a));
const ease = (n: number) => n * n * (3 - 2 * n);
export function blendPose(a: SvgPose, b: SvgPose, amount: number): SvgPose {
  if (amount <= 0) return a;
  if (amount >= 1) return b;
  const mix = (x: number, y: number) => x + (y - x) * amount;
  return Object.fromEntries(Object.entries(a).map(([key, value]) => [key, typeof value === 'number'
    ? mix(value, b.blade) : (value as Point).map((v, i) => mix(v, (b[key as keyof SvgPose] as Point)[i]))])) as unknown as SvgPose;
}

/**
 * 关键姿势表。刻意夸张：弓步压得更低，出招时躯干前探、后手反向甩开，
 * 接触帧的肢体比写实比例更长。写意风格不守解剖，只求剪影一眼读得懂。
 */
export function svgPose(frame: string, style: CombatStyle): SvgPose {
  const p: SvgPose = {
    head: [-5, -127], shoulder: [-5, -105], hip: [-13, -58],
    backElbow: [-33, -84], backHand: [-20, -72], elbow: [19, -86], hand: [41, -104],
    backKnee: [-31, -30], backFoot: [-46, -2], knee: [18, -33], foot: [33, -2],
    robe: [-34, -5], tassel: [-14, -16], blade: -28
  };
  if (style === 'sword') { p.hand = [33, -90]; p.elbow = [15, -83]; p.backHand = [-26, -112]; }
  if (/windup|prep/.test(frame)) {
    // 沉入后腿，拧腰蓄势，衣袂被前脚带起。
    p.head = [-29, -112]; p.shoulder = [-27, -88]; p.hip = [-25, -45];
    p.backKnee = [-52, -27]; p.backFoot = [-70, -2]; p.knee = [15, -22]; p.foot = [47, -2];
    p.elbow = [-57, -85]; p.hand = [-63, -116];
    p.backElbow = [8, -100]; p.backHand = [34, -128]; p.blade = -158;
    p.robe = [42, -12]; p.tassel = [22, -30];
    if (/punch/.test(frame)) { p.elbow = [-62, -70]; p.hand = [-40, -86]; }
    if (/heavy/.test(frame)) { p.elbow = [-63, -112]; p.hand = [-72, -150]; p.backHand = [36, -88]; p.robe = [26, -22]; }
    if (/kick/.test(frame)) {
      p.head = [-21, -126]; p.shoulder = [-18, -103]; p.hip = [-15, -59];
      p.knee = [25, -86]; p.foot = [8, -46]; p.backFoot = [-36, -2];
      p.elbow = [15, -129]; p.hand = [34, -153]; p.backElbow = [-45, -81]; p.backHand = [-61, -99];
      p.robe = [46, -18];
    }
    if (/thrust/.test(frame)) { p.hand = [-40, -84]; p.elbow = [-57, -65]; p.blade = -12; p.backHand = [29, -136]; p.robe = [36, -8]; }
    if (/chop/.test(frame)) { p.elbow = [-49, -132]; p.hand = [-23, -156]; p.blade = -112; p.robe = [30, -20]; }
    if (/rising/.test(frame)) { p.hand = [-35, -50]; p.elbow = [-54, -70]; p.blade = 28; p.backHand = [19, -148]; p.robe = [40, -4]; }
  }
  if (/strike/.test(frame)) {
    // 弓步深蹲，前手探到底，后手向后甩开配重，衣袂整片向后撕开。
    p.head = [20, -114]; p.shoulder = [12, -89]; p.hip = [-10, -44];
    p.backKnee = [-48, -25]; p.backFoot = [-80, -2]; p.knee = [38, -31]; p.foot = [58, -2];
    p.backElbow = [-25, -104]; p.backHand = [-63, -128];
    p.elbow = [44, -74]; p.hand = [80, -62]; p.blade = -4;
    p.robe = [-58, -14]; p.tassel = [-40, -34];
    if (/kick/.test(frame)) {
      p.head = [-41, -128]; p.shoulder = [-29, -104]; p.hip = [-8, -58];
      p.backElbow = [-64, -116]; p.backHand = [-88, -140];
      p.elbow = [4, -126]; p.hand = [29, -156];
      p.knee = [44, -74]; p.foot = [98, -62]; p.backKnee = [-24, -28]; p.backFoot = [-36, -2];
      p.robe = [-64, -22];
    }
    if (/heavy/.test(frame)) { p.head = [27, -102]; p.shoulder = [17, -76]; p.backHand = [-63, -148]; p.elbow = [52, -60]; p.robe = [-52, -20]; }
    if (/cut/.test(frame)) { p.blade = -12; p.backHand = [-66, -138]; }
    if (/rising/.test(frame)) { p.blade = -48; p.head = [5, -120]; p.shoulder = [3, -95]; p.backHand = [-64, -75]; p.robe = [-44, -26]; }
    if (/chop/.test(frame)) { p.blade = 32; p.backHand = [-58, -121]; p.robe = [-60, -8]; }
  }
  if (/close/.test(frame)) return blendPose(svgPose(frame.replace('close', 'strike'), style), p, 0.65);
  if (/hurt/.test(frame)) {
    p.head = [-25, -122]; p.shoulder = [-20, -99]; p.elbow = [4, -87]; p.hand = [17, -81];
    p.backElbow = [-41, -101]; p.backHand = [-46, -121]; p.blade = 30; p.robe = [48, -20]; p.tassel = [30, -34];
  }
  if (/dodge/.test(frame)) {
    p.head = [-52, -95]; p.shoulder = [-38, -75]; p.hip = [-14, -40];
    p.elbow = [-15, -96]; p.hand = [9, -118]; p.knee = [26, -23]; p.backHand = [-70, -50]; p.blade = -42;
    p.robe = [52, -28];
  }
  if (/guard|parry/.test(frame)) { p.elbow = [21, -85]; p.hand = [25, -119]; p.backHand = [10, -114]; p.blade = -79; p.robe = [26, -10]; }
  return p;
}

/** One compressed silhouette and one launch: no alternating walk cycle. */
export function sampleApproachPose(actionId: string, style: CombatStyle, phase: number, retreat = false): SvgPose {
  const idle = svgPose('idle', style);
  const movement = approachForAction(actionId);
  if (!movement) return idle;
  const p = svgPose('idle', style);
  // 压低身法前冲：头领在髋之前，但仍要站得住，不能读成摔倒。
  p.head = [34, -106]; p.shoulder = [20, -86]; p.hip = [-20, -45];
  p.knee = [16, -36]; p.foot = [38, -2];
  p.backKnee = [-50, -26]; p.backFoot = [-78, -5];
  p.elbow = [-6, -70]; p.hand = [14, -88]; p.backElbow = [-42, -72]; p.backHand = [-70, -90];
  p.robe = [-58, -20];
  let prepFrame = 'punch_windup';
  switch (movement.kind) {
    case 'step-in': p.hand = [30, -98]; break;
    case 'palm-drive': p.head = [38, -100]; p.shoulder = [22, -82]; p.hand = [2, -70]; prepFrame = 'heavy_windup'; break;
    case 'knee-hop': p.knee = [26, -70]; p.foot = [2, -38]; p.hand = [24, -118]; prepFrame = 'kick_windup'; break;
    case 'sword-glide': p.hand = [9, -74]; p.blade = -8; prepFrame = 'thrust_prep'; break;
    case 'cross-step': p.hand = [-33, -95]; p.blade = -165; prepFrame = 'cut_prep'; break;
    case 'raised-step': p.hand = [-14, -132]; p.elbow = [-36, -106]; p.blade = -130; prepFrame = 'chop_prep'; break;
    case 'low-skate': p.head = [32, -96]; p.shoulder = [16, -78]; p.hand = [-20, -48]; p.blade = 8; prepFrame = 'rising_prep'; break;
  }
  if (retreat) {
    p.head = [-28, -112]; p.shoulder = [-20, -91]; p.hand = [22, -100];
    return phase >= 1 ? idle : p;
  }
  // 身法只有两拍：扎住架势，然后换位。中间不做行走循环。
  if (phase < 0.12) return idle;
  return phase < 0.86 ? p : blendPose(p, svgPose(prepFrame, style), clamp((phase - .86) / .14));
}

/**
 * Shares the director's markers and hit stop; no CSS/SMIL animation clock.
 * 姿势之间是硬切，不是插值：蓄势定住、出招一帧到位、余劲再切、收势切回待机。
 */
export function sampleSvgPose(timeline: ResolvedBattleTimeline | null, side: BattleSide, elapsed: number, style: CombatStyle, frame: string, reduced = false): SvgPose {
  const idle = svgPose('idle', style);
  if (!timeline || timeline.kind === 'settlement' || timeline.kind === 'effect_tick') return idle;
  if (reduced) return svgPose(frame, style);
  const c = timeline.choreography;
  const arrival = timeline.actor.actionDelayMs;
  const impact = timeline.impactAtMs;
  const holdEnd = impact + c.hitStopMs;
  const t = elapsed >= impact && elapsed < holdEnd ? impact : elapsed;
  if (side !== timeline.actor.side) {
    const reaction = timeline.target.reaction;
    if (reaction === 'none' || reaction === 'effect') return idle;
    // 闪避提前，招架稍早，受击严格在接触帧之后；都是硬切到位。
    const at = reaction === 'dodge' ? Math.max(c.launchAtMs, impact - 95) : reaction === 'parry' ? Math.max(c.launchAtMs, impact - 50) : impact;
    if (t < at) return idle;
    const pulled = reaction === 'parry' ? 'guard' : reaction === 'hit' ? 'hurt' : 'dodge';
    const held = svgPose(pulled, style);
    // 收势：先切回半个待机，再切干净，避免直接弹回原姿势。
    if (t >= c.restAtMs) return blendPose(idle, held, 0.4);
    return held;
  }
  if (timeline.actor.motion === 'focus') {
    if (t < c.launchAtMs) return idle;
    return t < c.restAtMs ? svgPose('guard', style) : idle;
  }
  if (t < arrival) return sampleApproachPose(timeline.actor.visual.actionId, style, progress(t, 0, arrival));
  const frames = timeline.actor.visual.frames;
  const strikeIndex = frames.findIndex(f => f.frameId.includes('strike'));
  if (strikeIndex < 0) return svgPose(frame, style);
  const prep = svgPose(frames[Math.max(0, strikeIndex - 1)].frameId, style);
  const strike = svgPose(frames[strikeIndex].frameId, style);
  // 接触点。招架时留出一段距离，让剑停在格挡位置而不是穿进身体。
  const reach = timeline.result === 'parry' ? c.reach - 26 : c.reach;
  if (frames[strikeIndex].frameId.includes('kick')) strike.foot = [reach, c.contactY - 176];
  else if (style === 'fist') strike.hand = [reach, c.contactY - 176];
  else {
    const radians = strike.blade * Math.PI / 180;
    strike.hand = [reach - Math.cos(radians) * SVG_SWORD_LENGTH, c.contactY - 176 - Math.sin(radians) * SVG_SWORD_LENGTH];
    strike.elbow = [(strike.shoulder[0] + strike.hand[0]) / 2, strike.hand[1] + 10];
  }
  if (t < c.launchAtMs) return prep;
  // 出招一帧到位，并连同命中定格一起持住，定格结束才切到收招姿势。
  if (t < impact + c.hitStopMs) return strike;
  // 余劲：过接触帧后另起一个收招姿势，不回放释放动作。
  const finish = blendPose(strike, idle, 0.22);
  if (style === 'sword') {
    const rising = frames[strikeIndex].frameId.includes('rising');
    const thrust = frames[strikeIndex].frameId.includes('thrust');
    finish.hand = thrust ? [52, -84] : rising ? [14, -139] : [45, -70];
    finish.elbow = thrust ? [26, -82] : rising ? [31, -107] : [34, -75];
    finish.blade = thrust ? -9 : rising ? -145 : 48;
    finish.backHand = rising ? [-50, -112] : [-68, -104];
    finish.robe = rising ? [-40, -30] : [-50, -16];
  } else if (frames[strikeIndex].frameId.includes('kick')) {
    finish.knee = [30, -90]; finish.foot = [52, -55];
  } else {
    finish.hand = [66, -79]; finish.elbow = [38, -72]; finish.backHand = [-47, -133];
  }
  if (t < c.restAtMs) return finish;
  return idle;
}
