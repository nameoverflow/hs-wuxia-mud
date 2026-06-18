import { Container, Graphics, Sprite, Texture } from "pixi.js";
import type { ActorVisual } from "./animationTypes";
import { rigActionEntries } from "./rigActionCatalog";
import { createBattleActorRig, staticSkeletalAnimationEntries } from "./skeletal/catalog";
import segmentedPoseData from "./skeletal/data/segmented-v12-poses.json";
import { SkeletalCanvasRenderer } from "./skeletal/renderer";
import { interpolatePose, resolveRig } from "./skeletal/runtime";
import type { SkeletalPoseDefinition, SkeletonRigDefinition } from "./skeletal/types";

const actorPixelWidth = 340;
const actorPixelHeight = 248;
const actorDisplayWidth = 170;
const actorDisplayHeight = 124;
const skeletalAnimationEntries = [...rigActionEntries, ...staticSkeletalAnimationEntries];
const segmentedPoseLibrary = segmentedPoseData as Record<string, SkeletalPoseDefinition>;

export class PixiRigActor {
  readonly container = new Container();
  readonly sprite: Sprite;
  private readonly canvas = document.createElement("canvas");
  private readonly ctx: CanvasRenderingContext2D;
  private readonly renderer: SkeletalCanvasRenderer;
  private readonly texture: Texture;
  private visual: ActorVisual | null = null;
  private rig: SkeletonRigDefinition | null = null;
  private sequence: string[] = [];
  private renderKey = "";
  private durationMs = 720;
  private startedAt = 0;
  private lastRenderedAt = 0;
  private needsRender = true;

  constructor(side: "player" | "enemy") {
    this.canvas.width = actorPixelWidth;
    this.canvas.height = actorPixelHeight;
    const ctx = this.canvas.getContext("2d");
    if (!ctx) throw new Error("Failed to create Pixi rig actor canvas");
    this.ctx = ctx;
    this.renderer = new SkeletalCanvasRenderer(() => {
      this.needsRender = true;
    });
    this.texture = Texture.from(this.canvas);
    this.sprite = new Sprite(this.texture);
    this.sprite.anchor.set(0.5, 1);
    this.sprite.width = actorDisplayWidth;
    this.sprite.height = actorDisplayHeight;
    if (side === "enemy") this.sprite.scale.x *= -1;

    const shadow = new Graphics()
      .ellipse(0, -8, 42, 9)
      .fill({ color: 0x000000, alpha: 0.42 });
    shadow.y = 4;

    this.container.addChild(shadow, this.sprite);
  }

  setVisual(visual: ActorVisual, durationMs: number) {
    const sequence = visual.sequence?.length ? visual.sequence : [visual.poseId];
    const renderKey = `${visual.entryId}:${visual.actionId}:${sequence.join(",")}:${durationMs}`;
    if (renderKey === this.renderKey) return;

    const entry =
      skeletalAnimationEntries.find((candidate) => candidate.id === visual.entryId) ||
      skeletalAnimationEntries.find((candidate) => candidate.actionId === visual.actionId);
    this.visual = visual;
    this.rig = entry ? createBattleActorRig(entry, segmentedPoseLibrary) : null;
    this.sequence = sequence;
    this.durationMs = durationMs;
    this.renderKey = renderKey;
    this.startedAt = 0;
    this.lastRenderedAt = 0;
    this.needsRender = true;
  }

  update(now = performance.now()) {
    if (!this.visual || !this.rig) return;
    if (!this.startedAt) this.startedAt = now;
    const elapsed = now - this.startedAt;
    const animationMs = Math.max(120, this.durationMs);
    if (!this.needsRender && this.lastRenderedAt && now - this.lastRenderedAt < 1000 / 30 && elapsed < animationMs) return;

    this.lastRenderedAt = now;
    this.needsRender = false;
    const pose = this.poseAt(elapsed);
    const resolved = resolveRig(this.rig, pose, { bones: {}, bindings: {} });
    this.renderer.render(this.ctx, resolved, {
      showStage: false,
      showBones: false,
      showAnchors: false,
      showBindings: false,
      showImages: false,
      showSkin: true,
      showLabels: false,
      meshQuality: "full",
      zoom: 1.22,
      background: "transparent"
    });
    this.texture.source.update();
  }

  destroy() {
    this.texture.destroy(true);
    this.container.destroy({ children: true });
  }

  private poseAt(elapsedMs: number): SkeletalPoseDefinition {
    const rig = this.rig as SkeletonRigDefinition;
    const available = this.sequence.filter((poseId) => rig.poses[poseId]);
    if (available.length === 0) return rig.poses.idle || Object.values(rig.poses)[0];
    if (available.length === 1) return rig.poses[available[0]];

    const segmentMs = Math.max(80, this.durationMs / (available.length - 1));
    const segmentIndex = Math.min(available.length - 2, Math.floor(elapsedMs / segmentMs));
    const from = rig.poses[available[segmentIndex]];
    const to = rig.poses[available[segmentIndex + 1]];
    const t = easeInOut((elapsedMs - segmentIndex * segmentMs) / segmentMs);
    return interpolatePose(from, to, t, `${this.visual?.actionId || "rig"}.${segmentIndex}`);
  }
}

export function pixiActorSize() {
  return { width: actorDisplayWidth, height: actorDisplayHeight };
}

function easeInOut(value: number) {
  const t = Math.max(0, Math.min(1, value));
  return t * t * (3 - 2 * t);
}
