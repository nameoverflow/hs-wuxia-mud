let runtimePromise: Promise<PixiBattleRuntime> | null = null;

export type PixiBattleRuntime = {
  pixi: typeof import("pixi.js");
  gsap: typeof import("gsap").gsap;
  PixiRigActor: typeof import("./pixiRigActor").PixiRigActor;
  pixiActorSize: typeof import("./pixiRigActor").pixiActorSize;
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
      import("./pixiRigActor")
    ]).then(([pixi, gsapModule, rigModule]) => ({
      pixi,
      gsap: gsapModule.gsap,
      PixiRigActor: rigModule.PixiRigActor,
      pixiActorSize: rigModule.pixiActorSize
    }));
  return runtimePromise;
}
