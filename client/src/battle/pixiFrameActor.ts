import { Container, Graphics, Sprite, Texture } from "pixi.js";
import { clipFrameAt } from "./animationClip";
import type { ActorVisual, BattleAnimationFrame } from "./animationTypes";
import { frameTextureUrl } from "./frameCatalog";

const actorDisplayWidth = 170;
const actorDisplayHeight = 128;

export class PixiFrameActor {
  readonly container = new Container();
  private readonly visualRoot = new Container();
  private readonly hairSprite = new Sprite(Texture.EMPTY);
  private readonly bodySprite = new Sprite(Texture.EMPTY);
  private visual: ActorVisual | null = null;
  private frames: BattleAnimationFrame[] = [];
  private renderKey = "";
  private durationMs = 720;
  private delayMs = 0;
  private startedAt = 0;
  private lastFrameId = "";

  constructor(side: "player" | "enemy") {
    for (const sprite of [this.hairSprite, this.bodySprite]) {
      sprite.anchor.set(0.5, 1);
      sprite.width = actorDisplayWidth;
      sprite.height = actorDisplayHeight;
    }
    if (side === "enemy") this.visualRoot.scale.x = -1;
    this.visualRoot.addChild(this.hairSprite, this.bodySprite);

    const shadow = new Graphics().ellipse(0, -8, 42, 9).fill({ color: 0x000000, alpha: 0.42 });
    shadow.y = 4;
    this.container.addChild(shadow, this.visualRoot);
  }

  setVisual(visual: ActorVisual, durationMs: number, delayMs = 0) {
    const renderKey = `${visual.actionId}:${visual.profile}:${visual.frames.map((frame) => `${frame.frameId}:${frame.holdMs}`).join(",")}:${durationMs}:${delayMs}`;
    if (renderKey === this.renderKey) return;
    this.visual = visual;
    this.frames = visual.frames;
    this.durationMs = durationMs;
    this.delayMs = delayMs;
    this.renderKey = renderKey;
    this.startedAt = 0;
    this.lastFrameId = "";
  }

  update(now = performance.now()) {
    if (!this.visual) return;
    if (!this.startedAt) this.startedAt = now;
    const animationMs = Math.max(120, this.durationMs);
    const elapsed = Math.min(Math.max(0, now - this.startedAt - Math.max(0, this.delayMs)), animationMs);
    const { frame } = clipFrameAt(this.frames, elapsed, animationMs);
    if (frame.frameId === this.lastFrameId) return;
    this.lastFrameId = frame.frameId;
    this.bodySprite.texture = Texture.from(frameTextureUrl(this.visual.style, "body", frame.frameId));
    this.hairSprite.texture = Texture.from(frameTextureUrl(this.visual.style, "hair", frame.frameId));
    this.hairSprite.visible = this.visual.profile === "female";
  }

  destroy() {
    this.container.destroy({ children: true });
  }
}

export function frameActorSize() {
  return { width: actorDisplayWidth, height: actorDisplayHeight };
}
