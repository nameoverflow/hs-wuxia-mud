<script lang="ts">
  import { onDestroy, onMount } from "svelte";
  import type { Application, Container, Graphics } from "pixi.js";
  import { combatStyleFromSnapshot, idleVisualForStyle, visualProfileFromGender } from "../battle/rigActionCatalog";
  import type { ActorMotion, ActorVisual, BattleSide, ResolvedBattleTimeline, TargetReaction, TimelineVfx } from "../battle/animationTypes";
  import { loadPixiBattleRuntime } from "../battle/pixiBattleRuntime";
  import type { PixiRigActor } from "../battle/pixiRigActor";
  import type { GameState } from "../game";

  export let state: GameState;

  type BattleCombatant = GameState["battle"]["player"];
  type PixiApp = Application;
  type PixiModule = typeof import("pixi.js");
  type GsapApi = typeof import("gsap").gsap;
  type GsapTimeline = ReturnType<GsapApi["timeline"]>;
  type PixiRigActorCtor = typeof import("../battle/pixiRigActor").PixiRigActor;
  type PixiActorSize = typeof import("../battle/pixiRigActor").pixiActorSize;

  let pixi: PixiModule | null = null;
  let gsapApi: GsapApi | null = null;
  let PixiRigActorClass: PixiRigActorCtor | null = null;
  let pixiActorSize: PixiActorSize | null = null;
  let hostEl: HTMLDivElement;
  let app: PixiApp | null = null;
  let backgroundLayer: Graphics | null = null;
  let actorLayer: Container | null = null;
  let vfxLayer: Container | null = null;
  let textLayer: Container | null = null;
  let playerActor: PixiRigActor | null = null;
  let enemyActor: PixiRigActor | null = null;
  let resizeObserver: ResizeObserver | null = null;
  let cueTimeline: GsapTimeline | null = null;
  let ready = false;
  let lastTimelineId: number | null = null;
  let stageWidth = 0;
  let stageHeight = 0;

  $: player = state.battle.player;
  $: enemy = state.battle.enemy;
  $: battleTimeline = state.battle.animation.activeTimeline;
  $: if (ready) syncScene(player, enemy, battleTimeline, state.stats.gender);

  onMount(() => {
    let disposed = false;
    setup().then(() => {
      if (disposed) {
        teardown();
        return;
      }
      ready = true;
      syncScene(player, enemy, battleTimeline, state.stats.gender);
    });

    return () => {
      disposed = true;
      teardown();
    };
  });

  onDestroy(teardown);

  async function setup() {
    const runtime = await loadPixiBattleRuntime();
    pixi = runtime.pixi;
    gsapApi = runtime.gsap;
    PixiRigActorClass = runtime.PixiRigActor;
    pixiActorSize = runtime.pixiActorSize;

    app = new pixi.Application();
    await app.init({
      width: hostEl.clientWidth || 542,
      height: hostEl.clientHeight || 228,
      backgroundAlpha: 0,
      antialias: true,
      autoDensity: true,
      resolution: Math.min(window.devicePixelRatio || 1, 2)
    });

    backgroundLayer = new pixi.Graphics();
    actorLayer = new pixi.Container();
    vfxLayer = new pixi.Container();
    textLayer = new pixi.Container();
    playerActor = new PixiRigActorClass("player");
    enemyActor = new PixiRigActorClass("enemy");

    actorLayer.addChild(playerActor.container, enemyActor.container);
    app.stage.addChild(backgroundLayer, actorLayer, vfxLayer, textLayer);
    hostEl.appendChild(app.canvas);
    app.ticker.maxFPS = 60;
    app.ticker.add(tickActors);

    resizeObserver = new ResizeObserver(() => layoutStage(false));
    resizeObserver.observe(hostEl);
    layoutStage(true);
  }

  function requirePixi() {
    if (!pixi) throw new Error("PixiJS is not loaded");
    return pixi;
  }

  function requireGsap() {
    if (!gsapApi) throw new Error("GSAP is not loaded");
    return gsapApi;
  }

  function actorSourceSize() {
    return pixiActorSize?.() ?? { width: 170, height: 124 };
  }

  function syncScene(
    currentPlayer: BattleCombatant,
    currentEnemy: BattleCombatant,
    currentTimeline: ResolvedBattleTimeline | null,
    playerGender: string
  ) {
    if (!app || !playerActor || !enemyActor) return;
    const durationMs = currentTimeline?.durationMs ?? 720;
    const playerTiming = visualTimingFor("player", currentTimeline, durationMs);
    const enemyTiming = visualTimingFor("enemy", currentTimeline, durationMs);
    playerActor.setVisual(visualFor("player", currentTimeline, currentPlayer, currentEnemy, playerGender), playerTiming.durationMs, playerTiming.delayMs);
    enemyActor.setVisual(visualFor("enemy", currentTimeline, currentPlayer, currentEnemy, playerGender), enemyTiming.durationMs, enemyTiming.delayMs);

    const timelineId = currentTimeline?.id ?? null;
    if (timelineId !== lastTimelineId) {
      lastTimelineId = timelineId;
      playTimeline(currentTimeline);
    } else if (!currentTimeline) {
      resetActors();
    }
  }

  function visualFor(
    side: BattleSide,
    currentTimeline: ResolvedBattleTimeline | null,
    playerCombatant: BattleCombatant,
    enemyCombatant: BattleCombatant,
    playerGender: string
  ): ActorVisual {
    if (!currentTimeline) return idleVisualForSide(side, playerCombatant, enemyCombatant, playerGender);
    if (currentTimeline.actor.side === side) return currentTimeline.actor.visual;
    if (currentTimeline.target.side === side) return currentTimeline.target.visual;
    return idleVisualForSide(side, playerCombatant, enemyCombatant, playerGender);
  }

  function idleVisualForSide(
    side: BattleSide,
    playerCombatant: BattleCombatant,
    enemyCombatant: BattleCombatant,
    playerGender: string
  ): ActorVisual {
    const combatant = side === "player" ? playerCombatant : enemyCombatant;
    const gender = side === "player" ? combatant?.combatantSnapshotGender || playerGender : combatant?.combatantSnapshotGender;
    return idleVisualForStyle(
      combatStyleFromSnapshot(combatant?.combatantSnapshotCombatStyle),
      visualProfileFromGender(gender)
    );
  }

  function visualTimingFor(side: BattleSide, currentTimeline: ResolvedBattleTimeline | null, durationMs: number) {
    const delayMs = currentTimeline && !prefersReducedMotion() ? visualDelayMsFor(side, currentTimeline) : 0;
    return {
      durationMs: Math.max(120, durationMs - delayMs),
      delayMs
    };
  }

  function visualDelayMsFor(side: BattleSide, currentTimeline: ResolvedBattleTimeline) {
    if (currentTimeline.actor.side === side) return currentTimeline.actor.actionDelayMs;
    if (currentTimeline.target.side === side && currentTimeline.target.reaction !== "none" && currentTimeline.target.reaction !== "effect") {
      return currentTimeline.actor.actionDelayMs;
    }
    return 0;
  }

  function tickActors() {
    const now = performance.now();
    playerActor?.update(now);
    enemyActor?.update(now);
  }

  function layoutStage(reset = false) {
    if (!app || !backgroundLayer) return;
    const width = Math.max(1, Math.round(hostEl.clientWidth || app.renderer.width));
    const height = Math.max(1, Math.round(hostEl.clientHeight || app.renderer.height));
    stageWidth = width;
    stageHeight = height;
    app.renderer.resize(width, height);
    drawBackdrop(width, height);
    if (reset || !cueTimeline?.isActive()) resetActors();
  }

  function drawBackdrop(width: number, height: number) {
    if (!backgroundLayer) return;
    backgroundLayer.clear();
    backgroundLayer.rect(0, 0, width, height).fill({ color: 0x020303 });
    backgroundLayer.rect(0, height * 0.48, width, height * 0.52).fill({ color: 0x0a0b0b });
    backgroundLayer.rect(0, height * 0.48, width, 2).fill({ color: 0x232827, alpha: 0.72 });
    backgroundLayer.ellipse(width * 0.5, height * 0.74, width * 0.28, height * 0.12).fill({ color: 0xffd958, alpha: 0.08 });
    drawMist(width * 0.1, 42, width * 0.24, 18, 0.3);
    drawMist(width * 0.48, 38, width * 0.28, 22, 0.34);
    drawMist(width * 0.82, 48, width * 0.22, 16, 0.24);
    drawMist(width * 0.52, height - 30, width * 0.5, 18, 0.18);
  }

  function drawMist(x: number, y: number, width: number, height: number, alpha: number) {
    backgroundLayer?.ellipse(x, y, width, height).fill({ color: 0x69736c, alpha });
  }

  function playTimeline(timeline: ResolvedBattleTimeline | null) {
    cueTimeline?.kill();
    cueTimeline = null;
    clearPixiLayer(vfxLayer);
    clearPixiLayer(textLayer);
    resetActors();

    if (!timeline) return;

    const seconds = prefersReducedMotion() ? 0.01 : Math.max(0.12, timeline.durationMs / 1000);
    const animation = requireGsap().timeline({ defaults: { ease: "power2.out" } });
    cueTimeline = animation;

    if (timeline.kind === "settlement") {
      addSettlement(timeline, seconds, animation);
      return;
    }

    const actor = actorFor(timeline.actor.side);
    const target = actorFor(timeline.target.side);
    const actionDelaySeconds = actionDelaySecondsFor(timeline, seconds);
    const actionSeconds = Math.max(0.01, seconds - actionDelaySeconds);
    const impactAt = actionDelaySeconds + actionSeconds * 0.42;
    addActorMotion(animation, actor?.container, timeline.actor.motion, timeline.actor.side, seconds, actionDelaySeconds, actionSeconds);
    addTargetReaction(animation, target?.container, timeline.target.reaction, timeline.target.side, actionSeconds, impactAt);
    timeline.vfx.forEach((vfx) => addVfx(animation, vfx, timeline, actionSeconds, impactAt));
    if (timeline.floatText) addFloatText(animation, timeline.floatText, timeline.target.side, timeline.result, actionSeconds, impactAt);
  }

  function actorFor(side: BattleSide) {
    return side === "player" ? playerActor : enemyActor;
  }

  function resetActors() {
    const playerBase = actorBase("player");
    const enemyBase = actorBase("enemy");
    resetActor(playerActor, playerBase.x, playerBase.y);
    resetActor(enemyActor, enemyBase.x, enemyBase.y);
  }

  function resetActor(actor: PixiRigActor | null, x: number, y: number) {
    if (!actor) return;
    const scale = actorDisplayWidth() / actorSourceSize().width;
    actor.container.position.set(x, y);
    actor.container.scale.set(scale, scale);
    actor.container.angle = 0;
    actor.container.alpha = 1;
  }

  function addActorMotion(
    timeline: GsapTimeline,
    actor: Container | undefined,
    motion: ActorMotion,
    side: BattleSide,
    seconds: number,
    actionDelaySeconds: number,
    actionSeconds: number
  ) {
    if (!actor || motion === "none") return;
    const direction = side === "player" ? 1 : -1;
    const distance = strikeDistance() * direction;
    const recoil = strikeRecoil() * direction;
    const opposite = -10 * direction;

    if (motion === "focus") {
      timeline.to(actor, { y: actor.y - 5, duration: seconds * 0.46 }, 0);
      timeline.to(actor, { y: actor.y + 1, duration: seconds * 0.18 }, seconds * 0.46);
      timeline.to(actor, { y: actor.y, duration: seconds * 0.32 }, seconds * 0.64);
      return;
    }

    if (actionDelaySeconds > 0) {
      const origin = actorBase(side);
      const attack = actorEngagementPoint(side);
      const hopHeight = clamp(stageHeight * 0.18, 26, 46);
      const forwardPush = (motion === "drive" ? 22 : motion === "lunge" ? 16 : 10) * direction;
      const settleBack = (motion === "drive" ? 10 : 7) * direction;

      timeline.to(actor, { x: origin.x - 10 * direction, y: origin.y + 2, duration: actionDelaySeconds * 0.22, ease: "power2.out" }, 0);
      timeline.to(actor, { x: attack.x, y: attack.y - hopHeight, duration: actionDelaySeconds * 0.56, ease: "power2.inOut" }, actionDelaySeconds * 0.18);
      timeline.to(actor, { x: attack.x, y: attack.y, duration: actionDelaySeconds * 0.26, ease: "power2.in" }, actionDelaySeconds * 0.74);
      timeline.to(actor, { x: attack.x + forwardPush, y: attack.y - 3, duration: actionSeconds * 0.2, ease: "power3.out" }, actionDelaySeconds + actionSeconds * 0.12);
      timeline.to(actor, { x: attack.x - settleBack, y: attack.y, duration: actionSeconds * 0.18 }, actionDelaySeconds + actionSeconds * 0.42);
      timeline.to(actor, { x: origin.x, y: origin.y, duration: actionSeconds * 0.28, ease: "power2.inOut" }, actionDelaySeconds + actionSeconds * 0.72);
      return;
    }

    if (motion === "drive") {
      timeline.to(actor, { x: actor.x + opposite * 1.4, y: actor.y + 1, duration: seconds * 0.2 }, 0);
      timeline.to(actor, { x: actor.x + opposite * 1.8, y: actor.y + 2, duration: seconds * 0.24 }, seconds * 0.2);
      timeline.to(actor, { x: actor.x + distance, y: actor.y - 1, duration: seconds * 0.18, ease: "power4.in" }, seconds * 0.44);
      timeline.to(actor, { x: actor.x + recoil, y: actor.y, duration: seconds * 0.16 }, seconds * 0.62);
      timeline.to(actor, { x: actor.x, duration: seconds * 0.22 }, seconds * 0.78);
      return;
    }

    const lungeBoost = motion === "lunge" ? 12 * direction : 0;
    timeline.to(actor, { x: actor.x + opposite, duration: seconds * 0.24 }, 0);
    timeline.to(actor, { x: actor.x + distance + lungeBoost, y: actor.y - 3, duration: seconds * 0.28, ease: "power4.in" }, seconds * 0.24);
    timeline.to(actor, { x: actor.x + recoil, y: actor.y, duration: seconds * 0.22 }, seconds * 0.52);
    timeline.to(actor, { x: actor.x, duration: seconds * 0.26 }, seconds * 0.74);
  }

  function addTargetReaction(timeline: GsapTimeline, target: Container | undefined, reaction: TargetReaction, side: BattleSide, actionSeconds: number, impactAt: number) {
    if (!target || reaction === "none") return;
    const direction = side === "player" ? -1 : 1;

    if (reaction === "hit") {
      timeline.to(target, { x: target.x + 18 * direction, angle: 2 * direction, duration: actionSeconds * 0.13 }, impactAt);
      timeline.to(target, { x: target.x - 5 * direction, angle: 0, duration: actionSeconds * 0.1 }, impactAt + actionSeconds * 0.13);
      timeline.to(target, { x: target.x, duration: actionSeconds * 0.34 }, impactAt + actionSeconds * 0.23);
      return;
    }

    if (reaction === "dodge") {
      timeline.to(target, { x: target.x + 28 * direction, y: target.y + 4, alpha: 0.76, duration: actionSeconds * 0.18 }, impactAt);
      timeline.to(target, { x: target.x + 14 * direction, alpha: 1, duration: actionSeconds * 0.12 }, impactAt + actionSeconds * 0.18);
      timeline.to(target, { x: target.x, y: target.y, duration: actionSeconds * 0.32 }, impactAt + actionSeconds * 0.3);
      return;
    }

    if (reaction === "parry") {
      timeline.to(target, { x: target.x + 4 * direction, duration: actionSeconds * 0.12 }, impactAt);
      timeline.to(target, { x: target.x - 3 * direction, duration: actionSeconds * 0.1 }, impactAt + actionSeconds * 0.12);
      timeline.to(target, { x: target.x, duration: actionSeconds * 0.26 }, impactAt + actionSeconds * 0.22);
      return;
    }

    timeline.to(target, { y: target.y - 2, duration: actionSeconds * 0.18 }, impactAt);
    timeline.to(target, { y: target.y, duration: actionSeconds * 0.34 }, impactAt + actionSeconds * 0.18);
  }

  function addVfx(timeline: GsapTimeline, vfx: TimelineVfx, battleTimeline: ResolvedBattleTimeline, actionSeconds: number, impactAt: number) {
    if (!vfxLayer) return;
    const item = createVfx(vfx);
    const point = vfxPoint(vfx, battleTimeline);
    item.position.set(point.x, point.y);
    if (vfx.side === "enemy" && (vfx.kind === "trail" || vfx.kind === "parry")) item.scale.x = -1;
    vfxLayer.addChild(item);

    if (vfx.kind === "aura" || vfx.kind === "heal") {
      item.scale.set(0.64);
      timeline.to(item, { alpha: 0.82, duration: actionSeconds * 0.48 }, 0);
      timeline.to(item.scale, { x: 1.18, y: 1.18, duration: actionSeconds * 0.48 }, 0);
      timeline.to(item, { alpha: 0, duration: actionSeconds * 0.38 }, actionSeconds * 0.48);
      timeline.to(item.scale, { x: 1.42, y: 1.42, duration: actionSeconds * 0.38 }, actionSeconds * 0.48);
      return;
    }

    timeline.to(item, { alpha: 1, duration: actionSeconds * 0.1 }, impactAt);
    timeline.to(item.scale, { x: item.scale.x * 1.12, y: 1.12, duration: actionSeconds * 0.16 }, impactAt);
    timeline.to(item, { alpha: 0, duration: actionSeconds * 0.34 }, impactAt + actionSeconds * 0.16);
  }

  function createVfx(vfx: TimelineVfx) {
    const { Graphics } = requirePixi();
    const g = new Graphics();
    g.alpha = 0;

    if (vfx.kind === "trail" && vfx.variant === "stab-line") {
      g.poly([-52, -3, 32, -3, 54, 0, 32, 4, -52, 4], true).fill({ color: 0xff2b1c, alpha: 0.92 });
      g.poly([0, -2, 50, 0, 0, 2], true).fill({ color: 0xffae2a, alpha: 0.72 });
      return g;
    }

    if (vfx.kind === "trail" && vfx.variant === "uppercut-arc") {
      g.arc(0, 0, 48, Math.PI * 0.86, Math.PI * 1.82).stroke({ width: 4, color: 0xff2b1c, alpha: 0.9 });
      g.angle = 28;
      return g;
    }

    if (vfx.kind === "trail") {
      g.arc(0, 0, 50, Math.PI * 1.12, Math.PI * 1.92).stroke({ width: 4, color: 0xff2b1c, alpha: 0.9 });
      g.arc(0, 0, 38, Math.PI * 1.16, Math.PI * 1.76).stroke({ width: 2, color: 0xffae2a, alpha: 0.58 });
      g.angle = 18;
      return g;
    }

    if (vfx.kind === "parry") {
      g.arc(0, 0, 38, Math.PI * 0.72, Math.PI * 1.72).stroke({ width: 4, color: 0x5e9ce8, alpha: 0.92 });
      return g;
    }

    if (vfx.kind === "aura" || vfx.kind === "heal") {
      const color = vfx.kind === "heal" ? 0x89c686 : 0x5e9ce8;
      g.circle(0, 0, 45).stroke({ width: 2, color, alpha: 0.82 });
      g.circle(0, 0, 27).stroke({ width: 1, color, alpha: 0.38 });
      return g;
    }

    const color = vfx.variant === "dot-spark" ? 0x8f2824 : 0xff2f22;
    g.moveTo(-18, -11).lineTo(18, 11).stroke({ width: 4, color, alpha: 0.95 });
    g.moveTo(-12, 13).lineTo(14, -14).stroke({ width: 3, color: 0xffab22, alpha: 0.9 });
    return g;
  }

  function addFloatText(timeline: GsapTimeline, value: string, side: BattleSide, result: string, actionSeconds: number, impactAt: number) {
    if (!textLayer) return;
    const { Text } = requirePixi();
    const point = floatPoint(side);
    const text = new Text({
      text: value,
      style: {
        fontFamily: "ui-monospace, SFMono-Regular, Menlo, Consolas, monospace",
        fontSize: 18,
        fontWeight: "800",
        fill: result === "hit" ? 0xf0d7a2 : 0x5e9ce8,
        stroke: { color: 0x000000, width: 4 }
      },
      anchor: 0.5
    });
    text.position.set(point.x, point.y + 8);
    text.alpha = 0;
    text.scale.set(0.82);
    textLayer.addChild(text);

    timeline.to(text, { alpha: 1, y: point.y, duration: actionSeconds * 0.14 }, impactAt);
    timeline.to(text.scale, { x: 1.06, y: 1.06, duration: actionSeconds * 0.14 }, impactAt);
    timeline.to(text, { alpha: 0, y: point.y - 24, duration: actionSeconds * 0.42 }, impactAt + actionSeconds * 0.26);
  }

  function addSettlement(timeline: ResolvedBattleTimeline, seconds: number, animation: GsapTimeline) {
    if (!textLayer) return;
    const { Graphics, Text } = requirePixi();
    const overlay = new Graphics().rect(0, 0, stageWidth, stageHeight).fill({ color: 0x000000, alpha: 0.48 });
    const label = new Text({
      text: timeline.text,
      style: {
        fontFamily: "serif",
        fontSize: 26,
        fontWeight: "800",
        fill: 0xc8a75a,
        stroke: { color: 0x000000, width: 5 },
        align: "center"
      },
      anchor: 0.5
    });
    label.position.set(stageWidth / 2, stageHeight / 2);
    overlay.alpha = 0;
    label.alpha = 0;
    label.scale.set(0.92);
    textLayer.addChild(overlay, label);
    animation.to(overlay, { alpha: 1, duration: seconds * 0.3 }, 0);
    animation.to(label, { alpha: 1, duration: seconds * 0.36 }, seconds * 0.12);
    animation.to(label.scale, { x: 1.05, y: 1.05, duration: seconds * 0.38 }, seconds * 0.12);
    animation.to(label, { alpha: 0, duration: seconds * 0.28 }, seconds * 0.72);
    animation.to(overlay, { alpha: 0, duration: seconds * 0.28 }, seconds * 0.72);
  }

  function actorBase(side: BattleSide) {
    const margin = Math.max(18, stageWidth * 0.16);
    const halfActor = actorDisplayWidth() / 2;
    return {
      x: side === "player" ? margin + halfActor : stageWidth - margin - halfActor,
      y: stageHeight - 19
    };
  }

  function vfxPoint(vfx: TimelineVfx, timeline: ResolvedBattleTimeline | null = null) {
    const side = vfx.side === "center" ? "player" : vfx.side;
    const base =
      vfx.side === "center"
        ? { x: stageWidth / 2, y: stageHeight * 0.5 }
        : timeline && vfx.side === timeline.actor.side && isJumpMotion(timeline.actor.motion) && vfx.kind === "trail"
          ? actorEngagementPoint(timeline.actor.side)
          : actorBase(side);
    if (vfx.kind === "aura" || vfx.kind === "heal") return { x: base.x, y: stageHeight - 84 };
    if (vfx.kind === "parry") return { x: base.x, y: stageHeight - 108 };
    return { x: base.x, y: stageHeight - 104 };
  }

  function floatPoint(side: BattleSide) {
    return {
      x: actorBase(side).x,
      y: stageHeight - 126
    };
  }

  function actorDisplayWidth() {
    if (stageWidth <= 520) return 124;
    if (stageWidth <= 1180) return 148;
    return 170;
  }

  function strikeDistance() {
    return clamp(stageWidth * 0.24, 84, 148);
  }

  function strikeRecoil() {
    return clamp(stageWidth * 0.08, 28, 48);
  }

  function actorEngagementPoint(side: BattleSide) {
    const base = actorBase(side);
    const target = actorBase(side === "player" ? "enemy" : "player");
    const direction = side === "player" ? 1 : -1;
    const gap = actorDisplayWidth() * 0.34;
    const minX = Math.min(base.x, target.x);
    const maxX = Math.max(base.x, target.x);
    return {
      x: clamp(target.x - direction * gap, minX, maxX),
      y: base.y
    };
  }

  function actionDelaySecondsFor(timeline: ResolvedBattleTimeline, seconds: number) {
    if (prefersReducedMotion()) return 0;
    return Math.min(Math.max(0, timeline.actor.actionDelayMs) / 1000, seconds * 0.45);
  }

  function isJumpMotion(motion: ActorMotion) {
    return motion === "approach" || motion === "lunge" || motion === "drive";
  }

  function clamp(value: number, min: number, max: number) {
    return Math.max(min, Math.min(max, value));
  }

  function prefersReducedMotion() {
    return window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  }

  function clearPixiLayer(layer: Container | null) {
    if (!layer) return;
    for (const child of layer.removeChildren()) child.destroy();
  }

  function teardown() {
    cueTimeline?.kill();
    cueTimeline = null;
    resizeObserver?.disconnect();
    resizeObserver = null;
    if (app) app.ticker.remove(tickActors);
    if (playerActor) actorLayer?.removeChild(playerActor.container);
    if (enemyActor) actorLayer?.removeChild(enemyActor.container);
    playerActor?.destroy();
    enemyActor?.destroy();
    playerActor = null;
    enemyActor = null;
    app?.destroy({ removeView: true }, { children: true });
    app = null;
    backgroundLayer = null;
    actorLayer = null;
    vfxLayer = null;
    textLayer = null;
    ready = false;
  }
</script>

<div class="pixi-battle-stage" bind:this={hostEl} aria-hidden="true"></div>

<style>
  .pixi-battle-stage {
    position: absolute;
    inset: 0;
    overflow: hidden;
  }

  .pixi-battle-stage :global(canvas) {
    display: block;
    width: 100%;
    height: 100%;
  }
</style>
