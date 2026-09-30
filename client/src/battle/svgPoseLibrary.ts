import poseData from "../../../resources/scripts/combat_presentation/svg-poses.json";
import type { CombatStyle } from "./animationTypes";

export type Point = [number, number];

/** Joint positions in figure space (feet on y≈0, facing +x). Every segment has a fixed bone length. */
export interface SvgPose {
  /** 颈根：头从这里长出去。肩关节 shoulder 在它下面一点，手臂从胸口上沿长出来而不是从脖子上。 */
  neck: Point;
  head: Point; shoulder: Point; hip: Point;
  backElbow: Point; backHand: Point; elbow: Point; hand: Point;
  backKnee: Point; backFoot: Point; knee: Point; foot: Point;
  /** 剑穗方向点：剑穗从剑首朝这里垂下一小段。拳招不使用。 */
  tassel: Point;
  blade: number;
  /** 脚踩在地上：脚掌平贴地面。离地的脚（冲刺、踢腿、跳起）顺着小腿指出去。 */
  footPlanted: boolean;
  backFootPlanted: boolean;
  /** 手型：前手、后手分别是握拳、立掌还是剑指。 */
  hands: [HandShape, HandShape];
  /** 这副姿势用的骨长。 */
  bones: Bones;
}

export type HandShape = "fist" | "palm" | "finger";

/**
 * 一个关键姿势的参数：髋的位置、躯干/头/手臂的朝向（度，0 朝前、90 朝下），
 * 双脚落点和膝盖朝向。骨长是常量，所以任何姿势、任何混合都不会把肢体拉长。
 */
export interface PoseParams {
  hip: Point;
  torso: number;
  head: number;
  /** 前臂链：上臂、前臂的绝对朝向。 */
  arm: [number, number];
  backArm: [number, number];
  foot: Point;
  backFoot: Point;
  /** 膝盖弯向：1 朝前（+x），-1 朝后。 */
  knees: [number, number];
  blade: number;
  tassel: Point;
  /** 前手、后手的手型。 */
  hands: [HandShape, HandShape];
}

interface PoseDefinition {
  extends?: string;
  blend?: { from: string; to: string; amount: number };
  set?: Partial<PoseParams>;
  /** Per-rig overrides applied after set, e.g. how the sword hand differs. */
  rigs?: Partial<Record<CombatStyle, Partial<PoseParams>>>;
}

interface PoseLibrary {
  schemaVersion: number;
  bones: Bones;
  base: PoseParams;
  rigs: Record<CombatStyle, Partial<PoseParams>>;
  poses: Record<string, PoseDefinition>;
}

export interface Bones {
  torso: number; neck: number; headRadius: number;
  upperArm: number; forearm: number; thigh: number; shin: number;
}

const library = poseData as unknown as PoseLibrary;
export const bones: Bones = library.bones;
const rigs: CombatStyle[] = ["fist", "sword"];
const paramKeys = Object.keys(library.base) as (keyof PoseParams)[];

const rad = (deg: number) => deg * Math.PI / 180;
const dir = (deg: number): Point => [Math.cos(rad(deg)), Math.sin(rad(deg))];
const add = (a: Point, b: Point, k = 1): Point => [a[0] + b[0] * k, a[1] + b[1] * k];
const lerp = (a: number, b: number, t: number) => a + (b - a) * t;
/** 角度走最短弧。 */
const lerpAngle = (a: number, b: number, t: number) => a + ((((b - a) % 360) + 540) % 360 - 180) * t;

export function blendParams(a: PoseParams, b: PoseParams, amount: number): PoseParams {
  if (amount <= 0) return a;
  if (amount >= 1) return b;
  const point = (p: Point, q: Point): Point => [lerp(p[0], q[0], amount), lerp(p[1], q[1], amount)];
  return {
    hip: point(a.hip, b.hip),
    torso: lerpAngle(a.torso, b.torso, amount),
    head: lerpAngle(a.head, b.head, amount),
    arm: [lerpAngle(a.arm[0], b.arm[0], amount), lerpAngle(a.arm[1], b.arm[1], amount)],
    backArm: [lerpAngle(a.backArm[0], b.backArm[0], amount), lerpAngle(a.backArm[1], b.backArm[1], amount)],
    foot: point(a.foot, b.foot),
    backFoot: point(a.backFoot, b.backFoot),
    knees: amount < 0.5 ? a.knees : b.knees,
    blade: lerpAngle(a.blade, b.blade, amount),
    tassel: point(a.tassel, b.tassel),
    hands: amount < 0.5 ? a.hands : b.hands
  };
}

/**
 * 两段骨的解析反解：从 root 伸向 target，bend 决定关节弯向哪一侧。
 * 够不着时伸直指向目标，末端停在最远处。
 */
export function solveTwoBone(root: Point, target: Point, a: number, b: number, bend: number): { joint: Point; end: Point } {
  const dx = target[0] - root[0], dy = target[1] - root[1];
  const d = Math.max(Math.abs(a - b) + 0.01, Math.min(a + b - 0.01, Math.hypot(dx, dy)));
  const theta = Math.atan2(dy, dx);
  const alpha = Math.acos(Math.max(-1, Math.min(1, (a * a + d * d - b * b) / (2 * a * d))));
  const jointAngle = theta - Math.sign(bend || 1) * alpha;
  const joint: Point = [root[0] + Math.cos(jointAngle) * a, root[1] + Math.sin(jointAngle) * a];
  return { joint, end: [root[0] + Math.cos(theta) * d, root[1] + Math.sin(theta) * d] };
}

/** 关节弯向：target 在 root 正下方时，关节在前（+x）为 1。 */
export function bendOf(root: Point, joint: Point, end: Point) {
  const cross = (end[0] - root[0]) * (joint[1] - root[1]) - (end[1] - root[1]) * (joint[0] - root[0]);
  return cross < 0 ? 1 : -1;
}


/** 踩地时脚踝离地的高度：脚掌有厚度，脚踝不贴地。 */
export const ANKLE_HEIGHT = 5;
/** 肩关节在颈根下面多少：手臂从胸口上沿长出来。 */
const SHOULDER_DROP = 8;
/** 落点 y 在地面附近就算踩地。 */
const isPlanted = (foot: Point) => foot[1] > -4;
const ankleOf = (foot: Point): Point => (isPlanted(foot) ? [foot[0], foot[1] - ANKLE_HEIGHT] : foot);

/** 参数 → 关节坐标。脚是落点，髋够不着时自动下沉，宽马步自然就蹲低了。 */
export function poseFromParams(params: PoseParams): SvgPose {
  const b = bones;
  let hip: Point = [...params.hip];
  const reach = b.thigh + b.shin - 0.5;
  const frontAnkle = ankleOf(params.foot), backAnkle = ankleOf(params.backFoot);
  for (const foot of [frontAnkle, backAnkle]) {
    const dx = foot[0] - hip[0];
    if (Math.abs(dx) < reach) hip = [hip[0], Math.max(hip[1], foot[1] - Math.sqrt(reach * reach - dx * dx))];
  }
  const neck = add(hip, dir(params.torso), b.torso);
  const shoulder = add(neck, dir(params.torso), -SHOULDER_DROP);
  const head = add(neck, dir(params.head), b.neck + b.headRadius);
  const elbow = add(shoulder, dir(params.arm[0]), b.upperArm);
  const hand = add(elbow, dir(params.arm[1]), b.forearm);
  const backElbow = add(shoulder, dir(params.backArm[0]), b.upperArm);
  const backHand = add(backElbow, dir(params.backArm[1]), b.forearm);
  const front = solveTwoBone(hip, frontAnkle, b.thigh, b.shin, params.knees[0]);
  const back = solveTwoBone(hip, backAnkle, b.thigh, b.shin, params.knees[1]);
  return {
    neck, head, shoulder, hip, elbow, hand, backElbow, backHand,
    knee: front.joint, foot: front.end, backKnee: back.joint, backFoot: back.end,
    tassel: [...params.tassel], blade: params.blade,
    footPlanted: isPlanted(params.foot), backFootPlanted: isPlanted(params.backFoot),
    hands: params.hands, bones: b
  };
}

const resolved = new Map<string, PoseParams>();

function resolveParams(id: string, rig: CombatStyle, trail: string[] = []): PoseParams {
  const key = `${rig}/${id}`;
  const cached = resolved.get(key);
  if (cached) return cached;
  const definition = library.poses[id];
  if (!definition) throw new Error(`Missing SVG pose: ${id}${trail.length ? ` (via ${trail.join(" → ")})` : ""}`);
  if (trail.includes(id)) throw new Error(`SVG pose cycle: ${[...trail, id].join(" → ")}`);
  const next = [...trail, id];
  if (definition.extends && definition.blend) throw new Error(`SVG pose ${id} cannot both extend and blend`);
  const start = definition.blend
    ? blendParams(resolveParams(definition.blend.from, rig, next), resolveParams(definition.blend.to, rig, next), definition.blend.amount)
    : definition.extends ? resolveParams(definition.extends, rig, next) : { ...library.base, ...library.rigs[rig] };
  const params = { ...start, ...definition.set, ...definition.rigs?.[rig] } as PoseParams;
  resolved.set(key, params);
  return params;
}

export function hasSvgPose(id: string) {
  return Object.prototype.hasOwnProperty.call(library.poses, id);
}

export function svgPoseIds() {
  return Object.keys(library.poses);
}

export function poseParams(id: string, rig: CombatStyle): PoseParams {
  return resolveParams(id, rig);
}

/** 返回可修改的关节坐标：采样器会在接触帧上用反解把拳、脚、剑尖送到接触点。 */
export function svgPose(id: string, rig: CombatStyle): SvgPose {
  return poseFromParams(resolveParams(id, rig));
}

export function blendPose(fromId: string, toId: string, amount: number, rig: CombatStyle): SvgPose {
  return poseFromParams(blendParams(resolveParams(fromId, rig), resolveParams(toId, rig), amount));
}

/** 把整个姿势平移（身体顺着出招方向探出去）。 */
export function shiftPose(pose: SvgPose, dx: number, dy = 0): SvgPose {
  const move = (p: Point): Point => [p[0] + dx, p[1] + dy];
  return {
    ...pose,
    neck: move(pose.neck), head: move(pose.head), shoulder: move(pose.shoulder), hip: move(pose.hip),
    elbow: move(pose.elbow), hand: move(pose.hand), backElbow: move(pose.backElbow), backHand: move(pose.backHand),
    knee: move(pose.knee), foot: move(pose.foot), backKnee: move(pose.backKnee), backFoot: move(pose.backFoot),
    tassel: move(pose.tassel), blade: pose.blade
  };
}

/**
 * 把前手或前脚送到目标点，骨长不变：先在够不着时整个身体朝目标横移，再做两段反解。
 * 关节弯向沿用原姿势，所以手肘、膝盖不会翻到反方向。
 */
export function reachWith(pose: SvgPose, limb: "hand" | "foot", target: Point): SvgPose {
  const root = limb === "hand" ? pose.shoulder : pose.hip;
  const joint = limb === "hand" ? pose.elbow : pose.knee;
  const end = limb === "hand" ? pose.hand : pose.foot;
  const [a, b] = limb === "hand" ? [pose.bones.upperArm, pose.bones.forearm] : [pose.bones.thigh, pose.bones.shin];
  const bend = bendOf(root, joint, end);
  const length = a + b - 0.5;
  const dy = target[1] - root[1];
  let shifted = pose;
  const dx = target[0] - root[0];
  if (Math.hypot(dx, dy) > length && Math.abs(dy) < length) {
    shifted = shiftPose(pose, dx - Math.sign(dx || 1) * Math.sqrt(length * length - dy * dy));
  }
  const newRoot = limb === "hand" ? shifted.shoulder : shifted.hip;
  const solved = solveTwoBone(newRoot, target, a, b, bend);
  return limb === "hand"
    ? { ...shifted, elbow: solved.joint, hand: solved.end }
    : { ...shifted, knee: solved.joint, foot: solved.end, footPlanted: false };
}

if (library.schemaVersion !== 2) throw new Error(`Unsupported SVG pose schema ${library.schemaVersion}`);
for (const rig of rigs) if (!library.rigs[rig]) throw new Error(`SVG pose library has no rig ${rig}`);
for (const [id, definition] of Object.entries(library.poses)) {
  for (const layer of [definition.set, ...Object.values(definition.rigs || {})]) {
    for (const key of Object.keys(layer || {})) {
      if (!paramKeys.includes(key as keyof PoseParams)) throw new Error(`SVG pose ${id} sets unknown parameter ${key}`);
    }
  }
  for (const rig of rigs) resolveParams(id, rig);
}
