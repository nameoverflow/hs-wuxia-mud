import type { CombatStyle } from "./animationTypes";
import atlasData from "./frameAtlas.json";
import fistBody from "../assets/battle/ink-stage-v1/fist-body-atlas.png";
import fistHair from "../assets/battle/ink-stage-v1/fist-hair-atlas.png";
import swordBody from "../assets/battle/ink-stage-v1/sword-body-atlas.png";
import swordHair from "../assets/battle/ink-stage-v1/sword-hair-atlas.png";

type FrameLayer = "body" | "hair";
const urls = { fist: { body: fistBody, hair: fistHair }, sword: { body: swordBody, hair: swordHair } };
const atlases = atlasData as Record<CombatStyle, { width: number; height: number; frames: Record<string, { x: number; y: number }> }>;

export function frameAtlas(style: CombatStyle, layer: FrameLayer, frameId: string) {
  const atlas = atlases[style];
  const frame = atlas.frames[frameId];
  if (!frame) throw new Error(`Missing silhouette frame ${style}/${frameId}`);
  return { url: urls[style][layer], width: atlas.width, height: atlas.height, ...frame };
}

export function validateFrameAssets(style: CombatStyle, frameId: string) { frameAtlas(style, "body", frameId); }
export function allFrameTextureUrls() { return Object.values(urls).flatMap(layers => Object.values(layers)); }
