import backdrop from "../assets/battle/ink-stage-v1/backdrop.webp";

export const stageArt = { backdrop };
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
