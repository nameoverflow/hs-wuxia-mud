import { registerCustomVfx } from "./vfxRegistry";

const clamp = (v: number) => Math.max(0, Math.min(1, v));
const numberParam = (params: Record<string, unknown> | undefined, key: string, fallback: number) =>
  typeof params?.[key] === "number" && Number.isFinite(params[key]) ? (params[key] as number) : fallback;

/**
 * 剑网：以锚点为心，一笔一笔扇形展开的斩痕，前一半时间依次落笔，每笔起落各占一半生命。
 * params: count 笔数（1–12），spread 张角（度），stagger 落笔所占比例（0–1）。
 */
registerCustomVfx("blade_fan", ({ vfx, progress, direction, anchor, reduced }) => {
  const count = Math.round(Math.max(1, Math.min(12, numberParam(vfx.params, "count", 5))));
  const spread = numberParam(vfx.params, "spread", 240);
  const stagger = clamp(numberParam(vfx.params, "stagger", 0.5));
  const center = anchor(vfx.from ?? "contact");
  const life = 1 - stagger;
  return Array.from({ length: count }, (_, i) => {
    const local = clamp((progress - (count === 1 ? 0 : stagger * i / (count - 1))) / Math.max(0.01, life));
    const angle = -spread / 2 + spread * (count === 1 ? 0.5 : i / (count - 1));
    return {
      key: `${vfx.id}-${i}`,
      art: vfx.art,
      x: center.x,
      y: center.y,
      size: vfx.size ?? 240,
      scale: 0.7 + 0.4 * local,
      rotate: direction * (angle + (reduced ? 0 : 18 * local)),
      flip: direction,
      opacity: (vfx.opacity ?? 1) * Math.sin(Math.PI * local)
    };
  }).filter((sprite) => sprite.opacity > 0.001);
});
