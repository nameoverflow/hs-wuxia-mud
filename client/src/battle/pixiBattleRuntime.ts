import { allFrameTextureUrls } from "./frameCatalog";

let runtimePromise: Promise<PixiBattleRuntime> | null = null;

export type PixiBattleRuntime = {
  pixi: typeof import("pixi.js");
  gsap: typeof import("gsap").gsap;
  PixiFrameActor: typeof import("./pixiFrameActor").PixiFrameActor;
  frameActorSize: typeof import("./pixiFrameActor").frameActorSize;
};

export function preloadPixiBattleRuntime() {
  void loadPixiBattleRuntime();
}

export async function loadPixiBattleRuntime(): Promise<PixiBattleRuntime> {
  runtimePromise =
    runtimePromise ||
    Promise.all([
      import("pixi.js"),
      import("gsap"),
      import("./pixiFrameActor")
    ]).then(async ([pixi, gsapModule, frameModule]) => {
      await pixi.Assets.load(allFrameTextureUrls());
      return {
        pixi,
        gsap: gsapModule.gsap,
        PixiFrameActor: frameModule.PixiFrameActor,
        frameActorSize: frameModule.frameActorSize
      };
    });
  return runtimePromise;
}
