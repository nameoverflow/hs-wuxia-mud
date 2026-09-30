import type { ResolvedBattleTimeline, TimelineVfx, VfxAnchorRef, VfxArt } from "./animationTypes";
import type { BattleSceneSample } from "./battleDirector";

/** One ink sprite on the stage, in duel-world source px. The stage renders the list in order. */
export interface VfxSprite {
  key: string;
  art: VfxArt;
  x: number;
  y: number;
  /** Rendered box size before scale. */
  size: number;
  scale: number;
  rotate: number;
  /** 1 or -1: artwork is drawn facing right and mirrors with the attack direction. */
  flip: number;
  opacity: number;
}

export interface VfxContext {
  timeline: ResolvedBattleTimeline;
  scene: BattleSceneSample;
  vfx: TimelineVfx;
  elapsed: number;
  /** Hit-stop-warped time; everything freezes with the held contact. */
  visualTime: number;
  /** 0→1 through the sprite's life. */
  progress: number;
  direction: number;
  reduced: boolean;
  anchor: (ref: VfxAnchorRef) => { x: number; y: number };
}

export type CustomVfxSampler = (context: VfxContext) => VfxSprite[];

const customSamplers = new Map<string, CustomVfxSampler>();

/**
 * 给写不进数据的招式留一个口子：注册一个纯函数采样器，manifest 用 kind "custom" + effect 名引用。
 * 采样器只能读时间和时间线，所以暂停、慢放、拖动时间轴都照常工作。
 */
export function registerCustomVfx(name: string, sampler: CustomVfxSampler) {
  if (customSamplers.has(name)) throw new Error(`Custom VFX ${name} is already registered`);
  customSamplers.set(name, sampler);
}

export function customVfx(name: string) {
  return customSamplers.get(name);
}

export function hasCustomVfx(name: string) {
  return customSamplers.has(name);
}
