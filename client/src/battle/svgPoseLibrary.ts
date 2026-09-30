import poseData from "../../../resources/scripts/combat_poses/svg-poses.json";
import type { CombatStyle } from "./animationTypes";

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

/**
 * 关键姿势表的一条定义。姿势从 base + 骨架差异出发，
 * 或 extends 另一个姿势，或 blend 两个姿势；最后用 set 覆盖个别关节。
 */
interface PoseDefinition {
  extends?: string;
  blend?: { from: string; to: string; amount: number };
  set?: Partial<SvgPose>;
}

interface PoseLibrary {
  schemaVersion: number;
  joints: (keyof SvgPose)[];
  base: SvgPose;
  rigs: Record<CombatStyle, Partial<SvgPose>>;
  poses: Record<string, PoseDefinition>;
}

const library = poseData as unknown as PoseLibrary;
const rigs: CombatStyle[] = ["fist", "sword"];

export function blendPose(a: SvgPose, b: SvgPose, amount: number): SvgPose {
  if (amount <= 0) return a;
  if (amount >= 1) return b;
  const mix = (x: number, y: number) => x + (y - x) * amount;
  return Object.fromEntries(Object.entries(a).map(([key, value]) => [key, typeof value === "number"
    ? mix(value, b.blade) : (value as Point).map((v, i) => mix(v, (b[key as keyof SvgPose] as Point)[i]))])) as unknown as SvgPose;
}

const clonePose = (pose: SvgPose): SvgPose =>
  Object.fromEntries(Object.entries(pose).map(([key, value]) => [key, typeof value === "number" ? value : [...value]])) as unknown as SvgPose;

const resolved = new Map<string, SvgPose>();

function resolvePose(id: string, rig: CombatStyle, trail: string[] = []): SvgPose {
  const key = `${rig}/${id}`;
  const cached = resolved.get(key);
  if (cached) return cached;
  const definition = library.poses[id];
  if (!definition) throw new Error(`Missing SVG pose: ${id}${trail.length ? ` (via ${trail.join(" → ")})` : ""}`);
  if (trail.includes(id)) throw new Error(`SVG pose cycle: ${[...trail, id].join(" → ")}`);
  const next = [...trail, id];
  if (definition.extends && definition.blend) throw new Error(`SVG pose ${id} cannot both extend and blend`);
  const start = definition.blend
    ? blendPose(resolvePose(definition.blend.from, rig, next), resolvePose(definition.blend.to, rig, next), definition.blend.amount)
    : definition.extends ? resolvePose(definition.extends, rig, next) : { ...library.base, ...library.rigs[rig] };
  const pose = { ...start, ...definition.set } as SvgPose;
  resolved.set(key, pose);
  return pose;
}

export function hasSvgPose(id: string) {
  return Object.prototype.hasOwnProperty.call(library.poses, id);
}

/** 返回可修改的副本：采样器会在接触帧上改写手、脚的位置。 */
export function svgPose(id: string, rig: CombatStyle): SvgPose {
  return clonePose(resolvePose(id, rig));
}

if (library.schemaVersion !== 1) throw new Error(`Unsupported SVG pose schema ${library.schemaVersion}`);
for (const rig of rigs) if (!library.rigs[rig]) throw new Error(`SVG pose library has no rig ${rig}`);
for (const [id, definition] of Object.entries(library.poses)) {
  for (const joint of Object.keys({ ...definition.set })) {
    if (!library.joints.includes(joint as keyof SvgPose)) throw new Error(`SVG pose ${id} sets unknown joint ${joint}`);
  }
  for (const rig of rigs) resolvePose(id, rig);
}
