import { approachForAction } from "./battleApproach";
import type { BattleSide, CombatStyle, ResolvedBattleTimeline } from './animationTypes';

export const SVG_SWORD_LENGTH = 72;

type Point = [number, number];
export interface SvgPose {
  head: Point; shoulder: Point; hip: Point;
  backElbow: Point; backHand: Point; elbow: Point; hand: Point;
  backKnee: Point; backFoot: Point; knee: Point; foot: Point;
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
export function svgPose(frame: string, style: CombatStyle): SvgPose {
  const p: SvgPose = {
    head: [-5, -125], shoulder: [-5, -104], hip: [-12, -60],
    backElbow: [-31, -82], backHand: [-19, -72], elbow: [18, -84], hand: [39, -103],
    backKnee: [-29, -31], backFoot: [-42, -2], knee: [17, -34], foot: [31, -2], blade: -28
  };
  if (style === 'sword') { p.hand = [31, -88]; p.elbow = [14, -81]; p.backHand = [-25, -110]; }
  if (/windup|prep/.test(frame)) {
    // Sink into the rear leg; open the opposite arm to make the turn readable.
    p.head = [-27, -111]; p.shoulder = [-25, -87]; p.hip = [-23, -47];
    p.backKnee = [-48, -28]; p.backFoot = [-64, -2]; p.knee = [14, -24]; p.foot = [43, -2];
    p.elbow = [-53, -83]; p.hand = [-58, -112];
    p.backElbow = [6, -99]; p.backHand = [31, -126]; p.blade = -155;
    if (/punch/.test(frame)) { p.elbow = [-58, -69]; p.hand = [-38, -83]; }
    if (/heavy/.test(frame)) { p.elbow = [-59, -108]; p.hand = [-66, -144]; p.backHand = [34, -86]; }
    if (/kick/.test(frame)) {
      p.head = [-20, -124]; p.shoulder = [-17, -101]; p.hip = [-14, -61];
      p.knee = [23, -83]; p.foot = [7, -44]; p.backFoot = [-34, -2];
      p.elbow = [14, -126]; p.hand = [32, -149]; p.backElbow = [-43, -79]; p.backHand = [-58, -96];
    }
    if (/thrust/.test(frame)) { p.hand = [-38, -82]; p.elbow = [-53, -64]; p.blade = -12; p.backHand = [27, -133]; }
    if (/chop/.test(frame)) { p.elbow = [-46, -128]; p.hand = [-21, -151]; p.blade = -110; }
    if (/rising/.test(frame)) { p.hand = [-33, -48]; p.elbow = [-51, -68]; p.blade = 28; p.backHand = [17, -145]; }
  }
  if (/strike/.test(frame)) {
    // Full bow stance and a long counterbalancing arm, rather than two tucked fists.
    p.head = [17, -115]; p.shoulder = [10, -90]; p.hip = [-9, -47];
    p.backKnee = [-43, -27]; p.backFoot = [-72, -2]; p.knee = [35, -33]; p.foot = [54, -2];
    p.backElbow = [-23, -102]; p.backHand = [-58, -124];
    p.elbow = [40, -77]; p.hand = [72, -66]; p.blade = -4;
    if (/kick/.test(frame)) {
      p.head = [-37, -127]; p.shoulder = [-26, -103]; p.hip = [-7, -61];
      p.backElbow = [-59, -114]; p.backHand = [-81, -137];
      p.elbow = [3, -123]; p.hand = [26, -151];
      p.knee = [40, -70]; p.foot = [90, -60]; p.backKnee = [-22, -29]; p.backFoot = [-33, -2];
    }
    if (/heavy/.test(frame)) { p.head = [24, -104]; p.shoulder = [15, -80]; p.backHand = [-58, -143]; p.elbow = [47, -64]; }
    if (/cut/.test(frame)) { p.blade = -12; p.backHand = [-61, -134]; }
    if (/rising/.test(frame)) { p.blade = -48; p.head = [4, -121]; p.shoulder = [2, -97]; p.backHand = [-59, -77]; }
    if (/chop/.test(frame)) { p.blade = 32; p.backHand = [-54, -119]; }
  }
  if (/close/.test(frame)) return blendPose(svgPose(frame.replace('close', 'strike'), style), p, 0.65);
  if (/hurt/.test(frame)) {
    p.head = [-22, -121]; p.shoulder = [-18, -98]; p.elbow = [3, -85]; p.hand = [15, -80];
    p.backElbow = [-38, -99]; p.backHand = [-42, -119]; p.blade = 28;
  }
  if (/dodge/.test(frame)) {
    p.head = [-48, -94]; p.shoulder = [-35, -74]; p.hip = [-13, -42];
    p.elbow = [-13, -94]; p.hand = [8, -116]; p.knee = [24, -26]; p.backHand = [-65, -52]; p.blade = -40;
  }
  if (/guard|parry/.test(frame)) { p.elbow = [20, -83]; p.hand = [24, -116]; p.backHand = [9, -111]; p.blade = -77; }
  return p;
}

/** One compressed silhouette and one launch: no alternating walk cycle. */
export function sampleApproachPose(actionId: string, style: CombatStyle, phase: number, retreat = false): SvgPose {
  const idle = svgPose('idle', style);
  const movement = approachForAction(actionId);
  if (!movement) return idle;
  const p = svgPose('idle', style);
  // Head leads the hips by ~60px: the whole body commits to the dash.
  p.head = [38, -94]; p.shoulder = [22, -77]; p.hip = [-21, -43];
  p.knee = [13, -35]; p.foot = [34, -2];
  p.backKnee = [-48, -26]; p.backFoot = [-75, -5];
  p.elbow = [-4, -65]; p.hand = [15, -82]; p.backElbow = [-40, -66]; p.backHand = [-67, -84];
  let prepFrame = 'punch_windup';
  switch (movement.kind) {
    case 'step-in': p.hand = [31, -94]; break;
    case 'palm-drive': p.head = [42, -86]; p.shoulder = [24, -66]; p.hand = [4, -64]; prepFrame = 'heavy_windup'; break;
    case 'knee-hop': p.knee = [24, -64]; p.foot = [2, -34]; p.hand = [24, -113]; prepFrame = 'kick_windup'; break;
    case 'sword-glide': p.hand = [9, -68]; p.blade = -8; prepFrame = 'thrust_prep'; break;
    case 'cross-step': p.hand = [-31, -89]; p.blade = -165; prepFrame = 'cut_prep'; break;
    case 'raised-step': p.hand = [-13, -127]; p.elbow = [-34, -101]; p.blade = -130; prepFrame = 'chop_prep'; break;
    case 'low-skate': p.head = [36, -81]; p.shoulder = [18, -64]; p.hand = [-19, -43]; p.blade = 8; prepFrame = 'rising_prep'; break;
  }
  if (retreat) {
    p.head = [-28, -112]; p.shoulder = [-20, -91]; p.hand = [22, -100];
    return phase >= 1 ? idle : p;
  }
  // Pose-to-pose timing: hold a loaded silhouette, then catch into the attack.
  if (phase < 0.12) return idle;
  return phase < 0.86 ? p : blendPose(p, svgPose(prepFrame, style), ease(clamp((phase - .86) / .14)));
}

/** Shares the director's markers and hit stop; no CSS/SMIL animation clock. */
export function sampleSvgPose(timeline: ResolvedBattleTimeline | null, side: BattleSide, elapsed: number, style: CombatStyle, frame: string, reduced = false): SvgPose {
  const idle = svgPose('idle', style);
  if (!timeline || timeline.kind === 'settlement' || timeline.kind === 'effect_tick') return idle;
  if (reduced) return svgPose(frame, style);
  const c = timeline.choreography;
  const arrival = timeline.actor.actionDelayMs;
  const impact = timeline.impactAtMs;
  const t = elapsed >= impact && elapsed < impact + c.hitStopMs ? impact : elapsed;
  if (side !== timeline.actor.side) {
    const reaction = timeline.target.reaction;
    if (reaction === 'none' || reaction === 'effect') return idle;
    const at = reaction === 'dodge' ? Math.max(c.launchAtMs, impact - 95) : reaction === 'parry' ? Math.max(c.launchAtMs, impact - 50) : impact;
    const onset = reaction === 'hit' ? (t >= at ? 1 : 0) : ease(progress(t, at, impact));
    return blendPose(idle, svgPose(reaction === 'hit' ? 'hurt' : reaction === 'parry' ? 'guard' : 'dodge', style), onset * (1 - ease(progress(t, c.recoverAtMs, c.restAtMs))));
  }
  if (timeline.actor.motion === 'focus') {
    return blendPose(idle, svgPose('guard', style), ease(progress(t, 0, impact)) * (1 - ease(progress(t, c.recoverAtMs, c.restAtMs))));
  }
  if (t < arrival) return sampleApproachPose(timeline.actor.visual.actionId, style, progress(t, 0, arrival));
  const frames = timeline.actor.visual.frames;
  const strikeIndex = frames.findIndex(f => f.frameId.includes('strike'));
  if (strikeIndex < 0) return svgPose(frame, style);
  const prep = svgPose(frames[Math.max(0, strikeIndex - 1)].frameId, style);
  const strike = svgPose(frames[strikeIndex].frameId, style);
  // Fit the actual contact limb to the manifest reach, so all weapons meet the defender.
  if (frames[strikeIndex].frameId.includes('kick')) strike.foot = [c.reach, c.contactY - 176];
  else if (style === 'fist') strike.hand = [c.reach, c.contactY - 176];
  else {
    const radians = strike.blade * Math.PI / 180;
    strike.hand = [c.reach - Math.cos(radians) * SVG_SWORD_LENGTH, c.contactY - 176 - Math.sin(radians) * SVG_SWORD_LENGTH];
    strike.elbow = [(strike.shoulder[0] + strike.hand[0]) / 2, strike.hand[1] + 10];
  }
  if (t < c.launchAtMs) return prep;
  if (t < impact) return blendPose(prep, strike, progress(t, c.launchAtMs, impact) ** 3);
  // Continue the cut after contact, then gather into a composed ready stance.
  // A separate follow-through avoids rewinding the release animation.
  const finish = blendPose(strike, idle, 0.22);
  if (style === 'sword') {
    const rising = frames[strikeIndex].frameId.includes('rising');
    const thrust = frames[strikeIndex].frameId.includes('thrust');
    finish.hand = thrust ? [49, -82] : rising ? [13, -136] : [42, -68];
    finish.elbow = thrust ? [25, -80] : rising ? [30, -105] : [32, -73];
    finish.blade = thrust ? -9 : rising ? -142 : 46;
    finish.backHand = rising ? [-48, -109] : [-65, -102];
  } else if (frames[strikeIndex].frameId.includes('kick')) {
    finish.knee = [28, -88]; finish.foot = [48, -53];
  } else {
    finish.hand = [63, -77]; finish.elbow = [36, -70]; finish.backHand = [-44, -130];
  }
  const follow = ease(progress(t, impact + c.hitStopMs, c.recoverAtMs));
  if (t < c.recoverAtMs) return blendPose(strike, finish, follow);
  return blendPose(finish, idle, ease(progress(t, c.recoverAtMs, c.restAtMs)));
}
