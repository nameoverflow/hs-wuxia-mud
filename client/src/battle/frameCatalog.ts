import type { CombatStyle } from "./animationTypes";

type FrameLayer = "body" | "hair";

const frameModules = import.meta.glob("../assets/battle/actors/raster-v1/*/*/*.png", {
  eager: true,
  query: "?url",
  import: "default"
}) as Record<string, string>;

const frameUrls = new Map<string, string>();

for (const [path, url] of Object.entries(frameModules)) {
  const match = path.match(/raster-v1\/(fist|sword)\/(body|hair)\/([^/]+)\.png$/);
  if (!match) continue;
  frameUrls.set(frameKey(match[1] as CombatStyle, match[2] as FrameLayer, match[3]), url);
}

export function frameTextureUrl(style: CombatStyle, layer: FrameLayer, frameId: string) {
  const url = frameUrls.get(frameKey(style, layer, frameId));
  if (!url) throw new Error(`Missing raster ${layer} frame ${style}/${frameId}`);
  return url;
}

export function validateFrameAssets(style: CombatStyle, frameId: string) {
  frameTextureUrl(style, "body", frameId);
  frameTextureUrl(style, "hair", frameId);
}

function frameKey(style: CombatStyle, layer: FrameLayer, frameId: string) {
  return `${style}:${layer}:${frameId}`;
}
