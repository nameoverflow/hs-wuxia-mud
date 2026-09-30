import type { BattleSide, ResolvedBattleTimeline, TimelineVfx, VfxAnchorRef } from "./animationTypes";
import { sideHome, type BattleSceneSample } from "./battleDirector";
import { visualTimeAt } from "./battleTiming";
import { sampleSvgPose, SVG_SWORD_LENGTH } from "./svgBattlePose";
import { customVfx, type VfxContext, type VfxSprite } from "./vfxRegistry";
import "./customVfx";

export { registerCustomVfx, hasCustomVfx, type VfxSprite, type VfxContext, type CustomVfxSampler } from "./vfxRegistry";

const clamp = (v: number) => Math.max(0, Math.min(1, v));

/** Every ink layer for this instant: the built-in director layers first, then sprites, in manifest order. */
export function sampleVfx(timeline: ResolvedBattleTimeline | null, scene: BattleSceneSample, elapsed: number, reduced = false): VfxSprite[] {
  if (!timeline) return [];
  const sprites: VfxSprite[] = [];
  const direction = timeline.actor.side === "enemy" ? -1 : 1;
  const hit = timeline.hits[Math.max(0, scene.hitIndex)];
  if (scene.trail > 0) {
    // 斩击墨痕与招架墨环：锚点定位在接触点，图自身按攻击方向翻转。
    const art = hit.result === "parry" ? "parry" : timeline.vfx.find((v) => v.kind === "trail")?.art ?? "slash";
    sprites.push({ key: "arc", art, x: scene.contact.x, y: scene.contact.y, size: 200, scale: 1, rotate: scene.force >= 1 ? 8 : 0, flip: direction, opacity: scene.trail * 0.85 });
  }
  if (scene.burst > 0) {
    // 命中墨爆：先炸开再淡出，与接触点同步。
    const art = timeline.vfx.find((v) => v.kind === "impact")?.art ?? "impact";
    sprites.push({ key: "burst", art, x: scene.contact.x, y: scene.contact.y, size: 140, scale: (scene.force >= 1 ? 1.4 : 1) * (1.5 - scene.burst * 0.55), rotate: 0, flip: 1, opacity: scene.burst });
  }
  if (scene.aura > 0) {
    sprites.push({ key: "aura", art: "aura", x: sideHome(timeline.target.side), y: -66, size: 140, scale: 1, rotate: 0, flip: 1, opacity: scene.aura });
  }

  const visualTime = visualTimeAt(timeline, elapsed);
  const anchor = anchorResolver(timeline, scene, elapsed, reduced);
  for (const vfx of timeline.vfx) {
    if (vfx.kind !== "sprite" && vfx.kind !== "custom") continue;
    if (visualTime < vfx.startMs || visualTime >= vfx.endMs) continue;
    const anchoredHit = timeline.hits[Math.min(vfx.hit ?? 0, timeline.hits.length - 1)];
    if (vfx.results && !vfx.results.includes(anchoredHit.result)) continue;
    const progress = clamp((visualTime - vfx.startMs) / Math.max(1, vfx.endMs - vfx.startMs));
    if (vfx.kind === "custom") {
      const sampler = vfx.effect ? customVfx(vfx.effect) : undefined;
      if (!sampler) throw new Error(`Unknown custom VFX ${vfx.effect}`);
      sprites.push(...sampler({ timeline, scene, vfx, elapsed, visualTime, progress, direction, reduced, anchor }));
      continue;
    }
    sprites.push(sampleSprite(vfx, progress, visualTime, direction, reduced, anchor));
  }
  return sprites;
}

function sampleSprite(vfx: TimelineVfx, p: number, visualTime: number, direction: number, reduced: boolean, anchor: VfxContext["anchor"]): VfxSprite {
  const from = anchor(vfx.from ?? "contact");
  const to = vfx.to ? anchor(vfx.to) : from;
  const [scaleFrom, scaleTo] = vfx.scale ?? [1, 1];
  const fadeIn = vfx.fadeInMs ? clamp((visualTime - vfx.startMs) / vfx.fadeInMs) : 1;
  const fadeOut = vfx.fadeOutMs ? clamp((vfx.endMs - visualTime) / vfx.fadeOutMs) : 1;
  return {
    key: vfx.id,
    art: vfx.art,
    x: from.x + (to.x - from.x) * p,
    y: from.y + (to.y - from.y) * p,
    size: vfx.size ?? 200,
    scale: scaleFrom + (scaleTo - scaleFrom) * p,
    rotate: direction * ((vfx.rotate ?? 0) + (reduced ? 0 : (vfx.spin ?? 0) * p)),
    flip: direction,
    opacity: (vfx.opacity ?? 1) * fadeIn * fadeOut
  };
}

/** 锚点换算到舞台坐标：人物根节点加上当前姿势里的肢体位置，敌方镜像。 */
function anchorResolver(timeline: ResolvedBattleTimeline, scene: BattleSceneSample, elapsed: number, reduced: boolean) {
  const figureRoot = (side: BattleSide) => ({ x: sideHome(side) + scene[side].x, y: scene[side].y });
  return (ref: VfxAnchorRef) => {
    if (ref === "contact") return { ...scene.contact };
    if (ref === "center") return { x: 0, y: -70 };
    if (ref === "actor" || ref === "target") {
      const root = figureRoot(ref === "actor" ? timeline.actor.side : timeline.target.side);
      return { x: root.x, y: root.y - 70 };
    }
    const side = timeline.actor.side;
    const figure = scene[side];
    const root = figureRoot(side);
    const mirror = side === "enemy" ? -1 : 1;
    const pose = sampleSvgPose(timeline, side, elapsed, figure.visual.style, figure.frameId, reduced);
    const limb = ref === "actor.foot" ? pose.foot : pose.hand;
    const length = ref === "actor.blade" ? SVG_SWORD_LENGTH : 0;
    const angle = pose.blade * Math.PI / 180;
    return { x: root.x + mirror * (limb[0] + Math.cos(angle) * length), y: root.y + limb[1] + Math.sin(angle) * length };
  };
}
