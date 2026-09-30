import backdrop from "../assets/battle/ink-stage-v1/backdrop.webp";
import impact from "../assets/battle/ink-stage-v1/impact.webp";
import slash from "../assets/battle/ink-stage-v1/slash.webp";
import parry from "../assets/battle/ink-stage-v1/parry.webp";
import aura from "../assets/battle/ink-stage-v1/aura.webp";
import thrust from "../assets/battle/ink-stage-v1/thrust.webp";
import rising from "../assets/battle/ink-stage-v1/rising.webp";

/** 水墨特效素材。均为白墨透明底，贴在暗色舞台上。动作 manifest 的 vfx[].art 直接引用这里的键。 */
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

