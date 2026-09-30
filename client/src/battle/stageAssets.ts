import backdrop from "../assets/battle/ink-stage-v1/backdrop.webp";
import impact from "../assets/battle/ink-stage-v1/impact.webp";
import slash from "../assets/battle/ink-stage-v1/slash.webp";
import parry from "../assets/battle/ink-stage-v1/parry.webp";
import aura from "../assets/battle/ink-stage-v1/aura.webp";
import thrust from "../assets/battle/ink-stage-v1/thrust.webp";
import rising from "../assets/battle/ink-stage-v1/rising.webp";

/** 水墨特效素材。均为白墨透明底，贴在暗色舞台上。 */
export const stageArt = { backdrop, impact, slash, parry, aura, thrust, rising };
let loading: Promise<void> | null = null;

export function preloadBattleAssets() {
  if (typeof Image === "undefined") return Promise.resolve();
  if (!loading) {
    loading = Promise.all(Object.values(stageArt).map((url) => new Promise<void>((resolve, reject) => {
      const image = new Image();
      image.onload = () => { void image.decode().then(resolve, resolve); };
      image.onerror = () => reject(new Error(`Battle artwork could not load: ${url}`));
      image.src = url;
    }))).then(() => undefined).catch((error) => { loading = null; throw error; });
  }
  return loading;
}

/**
 * 把动作 manifest 里的 vfx 变体映射到实际素材。
 * 素材是按招式方向做的，所以挑图要以招式为主，动作类型只作兜底。
 */
export function vfxArt(kind: string, variant: string, actionId: string) {
  if (kind === "impact") return variant === "dot-spark" ? aura : impact;
  if (kind === "parry") return parry;
  if (kind === "aura" || kind === "heal") return aura;
  if (variant === "stab-line" || actionId.includes("thrust")) return thrust;
  if (actionId.includes("rising")) return rising;
  return slash;
}
