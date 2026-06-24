import { screenToRig } from "./math";
import type { Mat2D, ResolvedBinding, ResolvedRig, RigViewport, SkeletalRenderOptions, Vec2 } from "./types";

const defaultOptions: SkeletalRenderOptions = {
  showStage: true,
  showBones: true,
  showAnchors: true,
  showBindings: true,
  showImages: true,
  showSkin: true,
  showLabels: true,
  meshQuality: "full",
  fuseSegmentedSkin: true,
  zoom: 1,
  background: "#070808"
};

const alphaMaskCache = new WeakMap<HTMLImageElement, ImageAlphaMask>();
const compiledWarpSourceCache = new Map<string, CompiledWarpSource>();
const alphaTriangleCache = new WeakMap<ImageAlphaMask, Map<string, boolean[]>>();

export class SkeletalCanvasRenderer {
  private images = new Map<string, HTMLImageElement>();
  private onAssetLoad: () => void;

  constructor(onAssetLoad: () => void = () => undefined) {
    this.onAssetLoad = onAssetLoad;
  }

  async preloadImages(srcs: Array<string | undefined>): Promise<void> {
    const images = srcs.map((src) => this.loadImage(src)).filter((image): image is HTMLImageElement => !!image);
    await Promise.all(images.map((image) => waitForImage(image)));
  }

  render(ctx: CanvasRenderingContext2D, rig: ResolvedRig, options: Partial<SkeletalRenderOptions> = {}): RigViewport {
    const merged = { ...defaultOptions, ...options };
    const viewport = fitRigViewport(ctx.canvas.width, ctx.canvas.height, rig.definition.canvas, merged.zoom ?? 1);
    viewport.offsetX += merged.panX ?? 0;
    viewport.offsetY += merged.panY ?? 0;
    ctx.save();
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
    if (merged.showStage) {
      ctx.fillStyle = merged.background || "#070808";
      ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
      drawGrid(ctx, viewport, rig.definition.canvas);
    } else if (merged.background && merged.background !== "transparent") {
      ctx.fillStyle = merged.background;
      ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
    }
    ctx.setTransform(viewport.scale, 0, 0, viewport.scale, viewport.offsetX, viewport.offsetY);
    if (merged.showStage) drawStage(ctx, rig, viewport);

    for (const binding of rig.bindings) {
      if (binding.definition.tags?.includes("source") && !merged.showImages) continue;
      if (binding.definition.tags?.includes("skin") && !merged.showSkin) continue;
      drawBinding(ctx, rig, binding, viewport, merged, this.loadImage(binding.definition.image));
    }

    if (merged.fuseSegmentedSkin && hasSegmentedSkin(rig, merged)) {
      const bounds = segmentedSkinLayerBounds(ctx.canvas.width, ctx.canvas.height, rig, viewport);
      if (bounds) fuseSegmentedSkinLayer(ctx, bounds);
    }

    if (merged.showBindings) drawBindingLinks(ctx, rig, viewport, merged);
    if (merged.showBones) drawSkeletonGuides(ctx, rig, viewport, merged);
    if (merged.showAnchors) drawAnchors(ctx, rig, viewport, merged);

    ctx.restore();
    return viewport;
  }

  private loadImage(src: string | undefined) {
    if (!src) return null;
    const cached = this.images.get(src);
    if (cached) return cached;
    const image = new Image();
    image.onload = () => this.onAssetLoad();
    image.src = src;
    this.images.set(src, image);
    return image;
  }
}

function hasSegmentedSkin(rig: ResolvedRig, options: SkeletalRenderOptions) {
  if (!options.showSkin) return false;
  return rig.bindings.some((binding) => {
    const tags = binding.definition.tags || [];
    return binding.definition.opacity > 0.001 && tags.includes("segmented") && tags.includes("skin");
  });
}

interface PixelBounds {
  x: number;
  y: number;
  width: number;
  height: number;
}

function segmentedSkinLayerBounds(canvasWidth: number, canvasHeight: number, rig: ResolvedRig, viewport: RigViewport): PixelBounds | null {
  const points: Vec2[] = [];
  for (const binding of rig.bindings) {
    const tags = binding.definition.tags || [];
    if (binding.definition.opacity <= 0.001 || !tags.includes("segmented") || !tags.includes("skin")) continue;
    for (const point of segmentedSkinBindingPoints(rig, binding)) points.push(rigPointToScreen(point, viewport));
  }
  if (points.length === 0) return null;

  const padding = Math.max(14, Math.ceil(10 * viewport.scale));
  const raw = points.reduce(
    (acc, point) => ({
      minX: Math.min(acc.minX, point.x),
      minY: Math.min(acc.minY, point.y),
      maxX: Math.max(acc.maxX, point.x),
      maxY: Math.max(acc.maxY, point.y)
    }),
    { minX: Number.POSITIVE_INFINITY, minY: Number.POSITIVE_INFINITY, maxX: Number.NEGATIVE_INFINITY, maxY: Number.NEGATIVE_INFINITY }
  );
  const x = Math.max(0, Math.floor(raw.minX - padding));
  const y = Math.max(0, Math.floor(raw.minY - padding));
  const right = Math.min(canvasWidth, Math.ceil(raw.maxX + padding));
  const bottom = Math.min(canvasHeight, Math.ceil(raw.maxY + padding));
  if (right <= x || bottom <= y) return null;
  return { x, y, width: right - x, height: bottom - y };
}

function segmentedSkinBindingPoints(rig: ResolvedRig, binding: ResolvedBinding): Vec2[] {
  const def = binding.definition;
  if (def.kind === "mesh" && def.deform?.keypoints.length) {
    const points: Vec2[] = [];
    for (const keypoint of def.deform.keypoints) {
      const anchor = rig.anchorsById[keypoint.anchorId];
      if (!anchor) continue;
      const radius = Math.max(keypoint.radius, keypoint.sourceRadius ?? 0, 2);
      points.push(
        { x: anchor.position.x - radius, y: anchor.position.y - radius },
        { x: anchor.position.x + radius, y: anchor.position.y - radius },
        { x: anchor.position.x + radius, y: anchor.position.y + radius },
        { x: anchor.position.x - radius, y: anchor.position.y + radius }
      );
    }
    if (points.length > 0) return points;
  }
  return transformedBindingPoints(binding);
}

function transformedBindingPoints(binding: ResolvedBinding): Vec2[] {
  const def = binding.definition;
  const local =
    def.kind === "image" || def.kind === "mesh"
      ? [
          { x: -(def.pivotX ?? 0.5) * def.width, y: -(def.pivotY ?? 0.5) * def.height },
          { x: (1 - (def.pivotX ?? 0.5)) * def.width, y: -(def.pivotY ?? 0.5) * def.height },
          { x: (1 - (def.pivotX ?? 0.5)) * def.width, y: (1 - (def.pivotY ?? 0.5)) * def.height },
          { x: -(def.pivotX ?? 0.5) * def.width, y: (1 - (def.pivotY ?? 0.5)) * def.height }
        ]
      : [
          { x: -def.width * 0.5, y: -def.height * 0.5 },
          { x: def.width * 0.5, y: -def.height * 0.5 },
          { x: def.width * 0.5, y: def.height * 0.5 },
          { x: -def.width * 0.5, y: def.height * 0.5 }
        ];
  return local.map((point) => matrixPoint(binding.matrix, point));
}

function matrixPoint(matrix: Mat2D, point: Vec2): Vec2 {
  return {
    x: matrix.a * point.x + matrix.c * point.y + matrix.e,
    y: matrix.b * point.x + matrix.d * point.y + matrix.f
  };
}

function fuseSegmentedSkinLayer(ctx: CanvasRenderingContext2D, bounds: PixelBounds) {
  ctx.save();
  ctx.setTransform(1, 0, 0, 1, 0, 0);
  let imageData: ImageData;
  try {
    imageData = ctx.getImageData(bounds.x, bounds.y, bounds.width, bounds.height);
  } catch {
    ctx.restore();
    return;
  }

  const { width, height } = bounds;
  const source = imageData.data;
  const result = new Uint8ClampedArray(source);
  const target = { r: 250, g: 201, b: 28 };
  const targetLuma = 0.299 * target.r + 0.587 * target.g + 0.114 * target.b;

  for (let y = 0; y < height; y += 1) {
    for (let x = 0; x < width; x += 1) {
      const index = (y * width + x) * 4;
      const alpha = source[index + 3];
      if (alpha <= 8 || !isSkinColorPixel(source, index)) continue;

      const interior = isInteriorSkinPixel(source, width, height, x, y);
      const luma = 0.299 * source[index] + 0.587 * source[index + 1] + 0.114 * source[index + 2];
      const shade = interior ? 1 : clampNumber(luma / targetLuma, 0.82, 1.12);
      const normalizedR = clampNumber(target.r * shade, 0, 255);
      const normalizedG = clampNumber(target.g * shade, 0, 255);
      const normalizedB = clampNumber(target.b * shade, 0, 255);
      const blend = interior ? 1 : 0.42;

      result[index] = Math.round(lerpNumber(source[index], normalizedR, blend));
      result[index + 1] = Math.round(lerpNumber(source[index + 1], normalizedG, blend));
      result[index + 2] = Math.round(lerpNumber(source[index + 2], normalizedB, blend));
    }
  }

  imageData.data.set(result);
  ctx.putImageData(imageData, bounds.x, bounds.y);
  ctx.restore();
}

function isInteriorSkinPixel(data: Uint8ClampedArray, width: number, height: number, x: number, y: number) {
  const radius = 2;
  let skinNeighbors = 0;
  let sampledNeighbors = 0;
  for (let dy = -radius; dy <= radius; dy += 1) {
    for (let dx = -radius; dx <= radius; dx += 1) {
      if (dx === 0 && dy === 0) continue;
      const nx = x + dx;
      const ny = y + dy;
      if (nx < 0 || nx >= width || ny < 0 || ny >= height) return false;
      const index = (ny * width + nx) * 4;
      sampledNeighbors += 1;
      if (data[index + 3] >= 48 && isSkinColorPixel(data, index)) skinNeighbors += 1;
    }
  }
  return skinNeighbors >= sampledNeighbors * 0.72;
}

function isSkinColorPixel(data: Uint8ClampedArray, index: number) {
  const r = data[index];
  const g = data[index + 1];
  const b = data[index + 2];
  return r > 125 && g > 90 && b < 95 && r >= g * 0.9 && g > b * 1.55;
}

function waitForImage(image: HTMLImageElement): Promise<void> {
  if (image.complete && image.naturalWidth > 0) return Promise.resolve();
  return new Promise((resolve, reject) => {
    const previousLoad = image.onload;
    const previousError = image.onerror;
    image.onload = (event) => {
      if (typeof previousLoad === "function") previousLoad.call(image, event);
      resolve();
    };
    image.onerror = (event) => {
      if (typeof previousError === "function") previousError.call(image, event);
      reject(new Error("Failed to load skeletal render image"));
    };
  });
}

export function fitRigViewport(
  canvasWidth: number,
  canvasHeight: number,
  rigCanvas: { width: number; height: number },
  zoom = 1,
  padding = 26
): RigViewport {
  const scale = Math.min((canvasWidth - padding * 2) / rigCanvas.width, (canvasHeight - padding * 2) / rigCanvas.height) * zoom;
  return {
    scale,
    offsetX: (canvasWidth - rigCanvas.width * scale) / 2,
    offsetY: (canvasHeight - rigCanvas.height * scale) / 2
  };
}

export function rigPointToScreen(point: Vec2, viewport: RigViewport): Vec2 {
  return {
    x: point.x * viewport.scale + viewport.offsetX,
    y: point.y * viewport.scale + viewport.offsetY
  };
}

export function screenPointToRig(point: Vec2, viewport: RigViewport): Vec2 {
  return screenToRig(point, viewport.scale, viewport.offsetX, viewport.offsetY);
}

function drawGrid(ctx: CanvasRenderingContext2D, viewport: RigViewport, rigCanvas: { width: number; height: number; baseline: number }) {
  const left = viewport.offsetX;
  const top = viewport.offsetY;
  const width = rigCanvas.width * viewport.scale;
  const height = rigCanvas.height * viewport.scale;
  ctx.save();
  ctx.strokeStyle = "rgba(105, 115, 108, 0.22)";
  ctx.lineWidth = 1;
  for (let x = 0; x <= rigCanvas.width; x += 32) {
    const sx = left + x * viewport.scale;
    ctx.beginPath();
    ctx.moveTo(sx, top);
    ctx.lineTo(sx, top + height);
    ctx.stroke();
  }
  for (let y = 0; y <= rigCanvas.height; y += 32) {
    const sy = top + y * viewport.scale;
    ctx.beginPath();
    ctx.moveTo(left, sy);
    ctx.lineTo(left + width, sy);
    ctx.stroke();
  }
  ctx.strokeStyle = "rgba(200, 167, 90, 0.8)";
  ctx.beginPath();
  ctx.moveTo(left, top + rigCanvas.baseline * viewport.scale);
  ctx.lineTo(left + width, top + rigCanvas.baseline * viewport.scale);
  ctx.stroke();
  ctx.strokeStyle = "rgba(84, 198, 177, 0.72)";
  ctx.strokeRect(left, top, width, height);
  ctx.restore();
}

function drawStage(ctx: CanvasRenderingContext2D, rig: ResolvedRig, viewport: RigViewport) {
  ctx.save();
  ctx.strokeStyle = "rgba(255, 255, 255, 0.08)";
  ctx.lineWidth = 1 / viewport.scale;
  ctx.strokeRect(0, 0, rig.definition.canvas.width, rig.definition.canvas.height);
  ctx.restore();
}

function drawBinding(
  ctx: CanvasRenderingContext2D,
  rig: ResolvedRig,
  binding: ResolvedBinding,
  viewport: RigViewport,
  options: SkeletalRenderOptions,
  image: HTMLImageElement | null
) {
  const def = binding.definition;
  ctx.save();

  if (def.kind === "mesh") {
    ctx.globalAlpha = def.opacity;
    drawMeshBinding(ctx, rig, binding, viewport, options, image);
  } else if (def.kind === "image") {
    applyMatrix(ctx, binding.matrix);
    ctx.globalAlpha = def.opacity;
    if (image?.complete && image.naturalWidth > 0) {
      const pivotX = (def.pivotX ?? 0.5) * def.width;
      const pivotY = (def.pivotY ?? 0.5) * def.height;
      ctx.drawImage(image, -pivotX, -pivotY, def.width, def.height);
    } else {
      ctx.strokeStyle = "rgba(200, 167, 90, 0.65)";
      ctx.lineWidth = 1 / viewport.scale;
      ctx.strokeRect(-def.width * 0.5, -def.height * 0.5, def.width, def.height);
    }
  } else if (def.kind === "capsule") {
    applyMatrix(ctx, binding.matrix);
    ctx.globalAlpha = def.opacity;
    drawCapsule(ctx, def.width, def.height, def.color || "rgba(232, 225, 207, 0.5)", def.strokeColor, viewport.scale);
  } else if (def.kind === "circle") {
    applyMatrix(ctx, binding.matrix);
    ctx.globalAlpha = def.opacity;
    ctx.fillStyle = def.color || "rgba(232, 225, 207, 0.5)";
    ctx.beginPath();
    ctx.ellipse(0, 0, def.width * 0.5, def.height * 0.5, 0, 0, Math.PI * 2);
    ctx.fill();
  } else if (def.kind === "line") {
    applyMatrix(ctx, binding.matrix);
    ctx.globalAlpha = def.opacity;
    ctx.strokeStyle = def.color || "rgba(232, 225, 207, 0.62)";
    ctx.lineWidth = Math.max(1.2 / viewport.scale, def.height);
    ctx.beginPath();
    ctx.moveTo(0, 0);
    ctx.lineTo(def.width, 0);
    ctx.stroke();
  } else if (def.kind === "target") {
    applyMatrix(ctx, binding.matrix);
    ctx.globalAlpha = def.opacity;
    ctx.strokeStyle = def.color || "rgba(211, 93, 86, 0.8)";
    ctx.lineWidth = 1.4 / viewport.scale;
    ctx.beginPath();
    ctx.moveTo(-def.width * 0.5, 0);
    ctx.lineTo(def.width * 0.5, 0);
    ctx.moveTo(0, -def.height * 0.5);
    ctx.lineTo(0, def.height * 0.5);
    ctx.stroke();
  }

  ctx.restore();

  if (options.showLabels && options.selectedBindingId === binding.id) {
    drawLabel(ctx, binding.position, binding.name, "#c8a75a", viewport.scale);
  }
}

function drawMeshBinding(
  ctx: CanvasRenderingContext2D,
  rig: ResolvedRig,
  binding: ResolvedBinding,
  viewport: RigViewport,
  options: SkeletalRenderOptions,
  image: HTMLImageElement | null
) {
  const def = binding.definition;
  const deform = def.deform;
  if (!deform) return;

  const targetCenters = deform.keypoints.map((keypoint) => rig.anchorsById[keypoint.anchorId]?.position).filter((point): point is Vec2 => !!point);
  if (targetCenters.length !== deform.keypoints.length) return;

  const sourceCenters = deform.keypoints.map((keypoint) => ({ x: keypoint.sourceX, y: keypoint.sourceY }));
  const radii = deform.keypoints.map((keypoint) => keypoint.radius);
  const sourceRadii = deform.keypoints.map((keypoint) => keypoint.sourceRadius ?? keypoint.radius);
  const sourceSamples = ribbonSamples(sourceCenters, sourceRadii, deform.segments);
  const targetSamples = ribbonSamples(targetCenters, radii, deform.segments);
  const sourceRibbon = sourceSamples.map((sample) => ribbonPoint(sample.point, sample.tangent, sample.radius));
  const targetRibbon = targetSamples.map((sample) => ribbonPoint(sample.point, sample.tangent, sample.radius));

  if (image?.complete && image.naturalWidth > 0) {
    const meshSegments = options.meshQuality === "fast" ? Math.min(deform.segments ?? 12, 12) : deform.segments;
    const meshGridSize = options.meshQuality === "fast" ? Math.max(deform.gridSize ?? 2, 4) : deform.gridSize;
    const alphaMesh = buildSkeletonWarpMesh(image, def.width, def.height, sourceCenters, targetCenters, sourceRadii, radii, meshSegments, {
      algorithm: deform.algorithm,
      gridSize: meshGridSize,
      influence: deform.influence
    });
    if (alphaMesh) {
      drawTexturedGridTriangles(ctx, image, def.width, def.height, alphaMesh);
      return;
    }
    drawTexturedRibbonTriangles(ctx, image, def.width, def.height, sourceRibbon, targetRibbon);
    return;
  }

  ctx.strokeStyle = "rgba(200, 167, 90, 0.65)";
  ctx.lineWidth = 1 / viewport.scale;
  ctx.beginPath();
  traceRibbonPath(ctx, targetSamples, targetRibbon);
  ctx.stroke();
}

function drawTexturedRibbonTriangles(
  ctx: CanvasRenderingContext2D,
  image: HTMLImageElement,
  width: number,
  height: number,
  sourceRibbon: RibbonPoint[],
  targetRibbon: RibbonPoint[]
) {
  for (let i = 0; i < sourceRibbon.length - 1; i += 1) {
    const s0 = sourceRibbon[i].left;
    const s1 = sourceRibbon[i].right;
    const s2 = sourceRibbon[i + 1].left;
    const s3 = sourceRibbon[i + 1].right;
    const d0 = targetRibbon[i].left;
    const d1 = targetRibbon[i].right;
    const d2 = targetRibbon[i + 1].left;
    const d3 = targetRibbon[i + 1].right;
    drawTexturedTriangle(ctx, image, width, height, s0, s2, s1, d0, d2, d1);
    drawTexturedTriangle(ctx, image, width, height, s1, s2, s3, d1, d2, d3);
  }
}

export interface TexturedGridMesh {
  sourceRows: Vec2[][];
  targetRows: Vec2[][];
  mask?: ImageAlphaMask;
  alphaTriangles?: boolean[];
  sourceTriangles?: CompiledSourceTriangle[];
  targetPoints?: Vec2[];
}

interface SkeletonWarpOptions {
  algorithm?: "path" | "skinned";
  gridSize?: number;
  influence?: number;
}

interface CompiledWarpSource {
  key: string;
  sourceRows: Vec2[][];
  targetRows: Vec2[][];
  targetPoints: Vec2[];
  sourceTriangles: CompiledSourceTriangle[];
  pointPlans: CompiledPointPlan[];
  rowCount: number;
  columnCount: number;
}

type CompiledPointPlan = CompiledPathPointPlan | CompiledSkinnedPointPlan;

interface CompiledPathPointPlan {
  kind: "path";
  index: number;
  t: number;
  offset: number;
  invSourceRadius: number;
}

interface CompiledSkinnedPointPlan {
  kind: "skinned";
  original: Vec2;
  weightedSegments: CompiledWeightedSegment[];
  totalWeight: number;
  fallback: CompiledSegmentPlan | null;
}

interface CompiledSegmentPlan {
  index: number;
  t: number;
  sourceAlong: number;
  sourceOffset: number;
  invSourceLength: number;
  invSourceRadius: number;
}

interface CompiledWeightedSegment extends CompiledSegmentPlan {
  weight: number;
}

interface RuntimeTargetSegment {
  point: Vec2;
  directionX: number;
  directionY: number;
  normalX: number;
  normalY: number;
  vectorX: number;
  vectorY: number;
  length: number;
  radiusA: number;
  radiusB: number;
  radiusDelta: number;
}

interface CompiledSourceTriangle {
  s0: Vec2;
  s1: Vec2;
  s2: Vec2;
  d0: number;
  d1: number;
  d2: number;
  affine: SourceTriangleAffine;
}

interface SourceTriangleAffine {
  invDet: number;
  a0: number;
  a1: number;
  a2: number;
  c0: number;
  c1: number;
  c2: number;
  e0: number;
  e1: number;
  e2: number;
}

function drawTexturedGridTriangles(ctx: CanvasRenderingContext2D, image: HTMLImageElement, width: number, height: number, mesh: TexturedGridMesh) {
  if (mesh.sourceTriangles && mesh.targetPoints) {
    for (let triangleIndex = 0; triangleIndex < mesh.sourceTriangles.length; triangleIndex += 1) {
      if (mesh.alphaTriangles && !mesh.alphaTriangles[triangleIndex]) continue;
      const triangle = mesh.sourceTriangles[triangleIndex];
      drawTexturedTriangle(
        ctx,
        image,
        width,
        height,
        triangle.s0,
        triangle.s1,
        triangle.s2,
        mesh.targetPoints[triangle.d0],
        mesh.targetPoints[triangle.d1],
        mesh.targetPoints[triangle.d2],
        triangle.affine
      );
    }
    return;
  }

  const rowCount = Math.min(mesh.sourceRows.length, mesh.targetRows.length);
  let triangleIndex = 0;
  for (let row = 0; row < rowCount - 1; row += 1) {
    const sourceRow = mesh.sourceRows[row];
    const sourceNext = mesh.sourceRows[row + 1];
    const targetRow = mesh.targetRows[row];
    const targetNext = mesh.targetRows[row + 1];
    const columnCount = Math.min(sourceRow.length, sourceNext.length, targetRow.length, targetNext.length);
    for (let column = 0; column < columnCount - 1; column += 1) {
      const s0 = sourceRow[column];
      const s1 = sourceNext[column];
      const s2 = sourceRow[column + 1];
      const s3 = sourceNext[column + 1];
      const d0 = targetRow[column];
      const d1 = targetNext[column];
      const d2 = targetRow[column + 1];
      const d3 = targetNext[column + 1];
      if (mesh.alphaTriangles ? mesh.alphaTriangles[triangleIndex] : !mesh.mask || sourceTriangleHasAlpha(mesh.mask, s0, s1, s2)) {
        drawTexturedTriangle(ctx, image, width, height, s0, s1, s2, d0, d1, d2);
      }
      triangleIndex += 1;
      if (mesh.alphaTriangles ? mesh.alphaTriangles[triangleIndex] : !mesh.mask || sourceTriangleHasAlpha(mesh.mask, s2, s1, s3)) {
        drawTexturedTriangle(ctx, image, width, height, s2, s1, s3, d2, d1, d3);
      }
      triangleIndex += 1;
    }
  }
}

export function buildSkeletonWarpMesh(
  image: HTMLImageElement | null,
  width: number,
  height: number,
  sourceCenters: Vec2[],
  targetCenters: Vec2[],
  sourceRadii: number[],
  targetRadii: number[],
  segmentCount = 12,
  options: SkeletonWarpOptions = {}
): TexturedGridMesh | null {
  const mask = image ? getImageAlphaMask(image, width, height) : undefined;
  if ((image && !mask) || sourceCenters.length < 2 || sourceCenters.length !== targetCenters.length) return null;
  const algorithm = options.algorithm ?? "path";
  const sourceSamples = ribbonSamples(sourceCenters, sourceRadii, segmentCount);
  const targetSamples = ribbonSamples(targetCenters, targetRadii, segmentCount);
  const sampleCount = Math.min(sourceSamples.length, targetSamples.length);
  if (sampleCount < 2) return null;
  const sourcePath = sourceSamples.slice(0, sampleCount);
  const targetPath = targetSamples.slice(0, sampleCount);
  const targetSegments = compileTargetSegments(targetPath);
  const gridSize = Math.max(1.2, options.gridSize ?? (algorithm === "skinned" ? 2 : 3.5));
  const maxColumns = algorithm === "skinned" ? 96 : 28;
  const maxRows = algorithm === "skinned" ? 140 : 38;
  const columnCount = Math.max(5, Math.min(maxColumns, Math.ceil(width / gridSize) + 1));
  const rowCount = Math.max(5, Math.min(maxRows, Math.ceil(height / gridSize) + 1));
  const compiled = getCompiledWarpSource(width, height, sourceCenters, sourceRadii, sourcePath, segmentCount, algorithm, gridSize, options.influence ?? 2.6, rowCount, columnCount);

  for (let row = 0; row < rowCount; row += 1) {
    const targetRow = compiled.targetRows[row];
    for (let column = 0; column < columnCount; column += 1) {
      mapCompiledPointPlanInto(compiled.pointPlans[row * columnCount + column], targetSegments, targetRow[column]);
    }
  }

  return {
    sourceRows: compiled.sourceRows,
    targetRows: compiled.targetRows,
    sourceTriangles: compiled.sourceTriangles,
    targetPoints: compiled.targetPoints,
    ...(mask ? { mask, alphaTriangles: getAlphaTriangles(compiled, mask) } : {})
  };
}

function getCompiledWarpSource(
  width: number,
  height: number,
  sourceCenters: Vec2[],
  sourceRadii: number[],
  sourcePath: RibbonSample[],
  segmentCount: number,
  algorithm: "path" | "skinned",
  gridSize: number,
  influence: number,
  rowCount: number,
  columnCount: number
): CompiledWarpSource {
  const key = warpSourceCacheKey(width, height, sourceCenters, sourceRadii, sourcePath.length, segmentCount, algorithm, gridSize, influence, rowCount, columnCount);
  const cached = compiledWarpSourceCache.get(key);
  if (cached) return cached;

  const sourceRows: Vec2[][] = [];
  const targetRows: Vec2[][] = [];
  const targetPoints: Vec2[] = [];
  const pointPlans: CompiledPointPlan[] = [];
  for (let row = 0; row < rowCount; row += 1) {
    const sourceRow: Vec2[] = [];
    const targetRow: Vec2[] = [];
    const y = (height * row) / Math.max(1, rowCount - 1);
    for (let column = 0; column < columnCount; column += 1) {
      const source = {
        x: (width * column) / Math.max(1, columnCount - 1),
        y
      };
      sourceRow.push(source);
      const target = { x: 0, y: 0 };
      targetRow.push(target);
      targetPoints.push(target);
      pointPlans.push(algorithm === "skinned" ? compileSkinnedPointPlan(source, sourcePath, influence) : compilePathPointPlan(source, sourcePath));
    }
    sourceRows.push(sourceRow);
    targetRows.push(targetRow);
  }

  const sourceTriangles = compileSourceTriangles(sourceRows, rowCount, columnCount);
  const compiled = { key, sourceRows, targetRows, targetPoints, sourceTriangles, pointPlans, rowCount, columnCount };
  compiledWarpSourceCache.set(key, compiled);
  return compiled;
}

function compileSourceTriangles(sourceRows: Vec2[][], rowCount: number, columnCount: number): CompiledSourceTriangle[] {
  const triangles: CompiledSourceTriangle[] = [];
  for (let row = 0; row < rowCount - 1; row += 1) {
    const sourceRow = sourceRows[row];
    const sourceNext = sourceRows[row + 1];
    for (let column = 0; column < columnCount - 1; column += 1) {
      const topLeft = row * columnCount + column;
      const bottomLeft = (row + 1) * columnCount + column;
      const topRight = topLeft + 1;
      const bottomRight = bottomLeft + 1;
      triangles.push(
        compileSourceTriangle(sourceRow[column], sourceNext[column], sourceRow[column + 1], topLeft, bottomLeft, topRight),
        compileSourceTriangle(sourceRow[column + 1], sourceNext[column], sourceNext[column + 1], topRight, bottomLeft, bottomRight)
      );
    }
  }
  return triangles;
}

function compileSourceTriangle(s0: Vec2, s1: Vec2, s2: Vec2, d0: number, d1: number, d2: number): CompiledSourceTriangle {
  return { s0, s1, s2, d0, d1, d2, affine: sourceTriangleAffine(s0, s1, s2) };
}

function compilePathPointPlan(point: Vec2, sourcePath: RibbonSample[]): CompiledPathPointPlan {
  const projection = closestSourcePathProjection(point, sourcePath);
  const sourceA = sourcePath[projection.index];
  const sourceB = sourcePath[projection.index + 1];
  return {
    kind: "path",
    index: projection.index,
    t: projection.t,
    offset: projection.offset,
    invSourceRadius: 1 / Math.max(0.001, lerpNumber(sourceA.radius, sourceB.radius, projection.t))
  };
}

function compileSkinnedPointPlan(point: Vec2, sourcePath: RibbonSample[], influence: number): CompiledSkinnedPointPlan {
  let totalWeight = 0;
  let fallback: (CompiledSegmentPlan & { distanceSq: number }) | null = null;
  const weightedSegments: CompiledWeightedSegment[] = [];

  for (let index = 0; index < sourcePath.length - 1; index += 1) {
    const sourceA = sourcePath[index];
    const sourceB = sourcePath[index + 1];
    const projection = projectPointToSampleSegment(point, sourceA, sourceB);
    if (!projection) continue;
    const segmentPlan = compileSegmentPlan(point, sourceA, sourceB, projection.unclampedT, index);
    if (!fallback || projection.distanceSq < fallback.distanceSq) fallback = { ...segmentPlan, distanceSq: projection.distanceSq };

    const sourceRadius = Math.max(0.35, lerpNumber(sourceA.radius, sourceB.radius, projection.t));
    const influenceRadius = Math.max(sourceRadius * influence, 2.4);
    const distance = Math.sqrt(projection.distanceSq);
    const normalizedDistance = distance / influenceRadius;
    const outside = projection.unclampedT < 0 ? -projection.unclampedT : projection.unclampedT > 1 ? projection.unclampedT - 1 : 0;
    const longitudinalPenalty = 1 / (1 + outside * outside * 36);
    const weight = (longitudinalPenalty * longitudinalPenalty) / Math.max(0.0001, 0.035 + normalizedDistance ** 4);
    if (weight <= 0.000001) continue;

    totalWeight += weight;
    weightedSegments.push({ ...segmentPlan, weight });
  }

  if (totalWeight > 0.000001) {
    for (const segment of weightedSegments) segment.weight /= totalWeight;
  }

  return {
    kind: "skinned",
    original: point,
    weightedSegments,
    totalWeight,
    fallback: fallback ? stripDistance(fallback) : null
  };
}

function compileSegmentPlan(point: Vec2, sourceA: RibbonSample, sourceB: RibbonSample, t: number, index: number): CompiledSegmentPlan {
  const sourceVector = { x: sourceB.point.x - sourceA.point.x, y: sourceB.point.y - sourceA.point.y };
  const sourceLength = Math.max(0.001, Math.hypot(sourceVector.x, sourceVector.y));
  const sourceDirection = { x: sourceVector.x / sourceLength, y: sourceVector.y / sourceLength };
  const sourceNormal = normalForTangent(sourceDirection);
  const relative = { x: point.x - sourceA.point.x, y: point.y - sourceA.point.y };
  const radiusT = clampNumber(t, 0, 1);
  const sourceRadius = Math.max(0.001, lerpNumber(sourceA.radius, sourceB.radius, radiusT));
  return {
    index,
    t: radiusT,
    sourceAlong: relative.x * sourceDirection.x + relative.y * sourceDirection.y,
    sourceOffset: relative.x * sourceNormal.x + relative.y * sourceNormal.y,
    invSourceLength: 1 / sourceLength,
    invSourceRadius: 1 / sourceRadius
  };
}

function mapCompiledPointPlanInto(plan: CompiledPointPlan, targetSegments: RuntimeTargetSegment[], out: Vec2) {
  if (plan.kind === "path") {
    mapCompiledPathPointInto(plan, targetSegments, out);
    return;
  }
  if (plan.totalWeight <= 0.000001) {
    if (plan.fallback) {
      mapCompiledSegmentInto(plan.fallback, targetSegments, out);
    } else {
      out.x = plan.original.x;
      out.y = plan.original.y;
    }
    return;
  }

  let mappedX = 0;
  let mappedY = 0;
  for (const segment of plan.weightedSegments) {
    const target = targetSegments[segment.index];
    const targetRadius = Math.max(0.001, target.radiusA + target.radiusDelta * segment.t);
    const alongScale = target.length * segment.invSourceLength;
    const normalScale = targetRadius * segment.invSourceRadius;
    mappedX += (target.point.x + target.directionX * segment.sourceAlong * alongScale + target.normalX * segment.sourceOffset * normalScale) * segment.weight;
    mappedY += (target.point.y + target.directionY * segment.sourceAlong * alongScale + target.normalY * segment.sourceOffset * normalScale) * segment.weight;
  }
  out.x = mappedX;
  out.y = mappedY;
}

function mapCompiledPathPointInto(plan: CompiledPathPointPlan, targetSegments: RuntimeTargetSegment[], out: Vec2) {
  const target = targetSegments[plan.index];
  const targetRadius = Math.max(0.001, target.radiusA + target.radiusDelta * plan.t);
  const radiusScale = targetRadius * plan.invSourceRadius;
  out.x = target.point.x + target.vectorX * plan.t + target.normalX * plan.offset * radiusScale;
  out.y = target.point.y + target.vectorY * plan.t + target.normalY * plan.offset * radiusScale;
}

function mapCompiledSegmentInto(plan: CompiledSegmentPlan, targetSegments: RuntimeTargetSegment[], out: Vec2) {
  const target = targetSegments[plan.index];
  const targetRadius = Math.max(0.001, target.radiusA + target.radiusDelta * plan.t);
  const alongScale = target.length * plan.invSourceLength;
  const normalScale = targetRadius * plan.invSourceRadius;
  out.x = target.point.x + target.directionX * plan.sourceAlong * alongScale + target.normalX * plan.sourceOffset * normalScale;
  out.y = target.point.y + target.directionY * plan.sourceAlong * alongScale + target.normalY * plan.sourceOffset * normalScale;
}

function compileTargetSegments(targetPath: RibbonSample[]): RuntimeTargetSegment[] {
  const segments: RuntimeTargetSegment[] = [];
  for (let index = 0; index < targetPath.length - 1; index += 1) {
    const targetA = targetPath[index];
    const targetB = targetPath[index + 1];
    const vectorX = targetB.point.x - targetA.point.x;
    const vectorY = targetB.point.y - targetA.point.y;
    const length = Math.max(0.001, Math.hypot(vectorX, vectorY));
    const directionX = vectorX / length;
    const directionY = vectorY / length;
    segments.push({
      point: targetA.point,
      directionX,
      directionY,
      normalX: -directionY,
      normalY: directionX,
      vectorX,
      vectorY,
      length,
      radiusA: targetA.radius,
      radiusB: targetB.radius,
      radiusDelta: targetB.radius - targetA.radius
    });
  }
  return segments;
}

function getAlphaTriangles(compiled: CompiledWarpSource, mask: ImageAlphaMask): boolean[] {
  let bySource = alphaTriangleCache.get(mask);
  if (!bySource) {
    bySource = new Map<string, boolean[]>();
    alphaTriangleCache.set(mask, bySource);
  }
  const cached = bySource.get(compiled.key);
  if (cached) return cached;

  const alphaTriangles: boolean[] = [];
  for (let row = 0; row < compiled.rowCount - 1; row += 1) {
    const sourceRow = compiled.sourceRows[row];
    const sourceNext = compiled.sourceRows[row + 1];
    for (let column = 0; column < compiled.columnCount - 1; column += 1) {
      const s0 = sourceRow[column];
      const s1 = sourceNext[column];
      const s2 = sourceRow[column + 1];
      const s3 = sourceNext[column + 1];
      alphaTriangles.push(sourceTriangleHasAlpha(mask, s0, s1, s2), sourceTriangleHasAlpha(mask, s2, s1, s3));
    }
  }

  bySource.set(compiled.key, alphaTriangles);
  return alphaTriangles;
}

function stripDistance(segment: CompiledSegmentPlan & { distanceSq: number }): CompiledSegmentPlan {
  return {
    index: segment.index,
    t: segment.t,
    sourceAlong: segment.sourceAlong,
    sourceOffset: segment.sourceOffset,
    invSourceLength: segment.invSourceLength,
    invSourceRadius: segment.invSourceRadius
  };
}

function warpSourceCacheKey(
  width: number,
  height: number,
  sourceCenters: Vec2[],
  sourceRadii: number[],
  sourcePathLength: number,
  segmentCount: number,
  algorithm: "path" | "skinned",
  gridSize: number,
  influence: number,
  rowCount: number,
  columnCount: number
) {
  return [
    algorithm,
    numberCacheKey(width),
    numberCacheKey(height),
    segmentCount,
    numberCacheKey(gridSize),
    numberCacheKey(influence),
    sourcePathLength,
    rowCount,
    columnCount,
    pointsCacheKey(sourceCenters),
    numbersCacheKey(sourceRadii)
  ].join("|");
}

function pointsCacheKey(points: Vec2[]) {
  return points.map((point) => `${numberCacheKey(point.x)},${numberCacheKey(point.y)}`).join(";");
}

function numbersCacheKey(values: number[]) {
  return values.map(numberCacheKey).join(",");
}

function numberCacheKey(value: number) {
  return Number.isFinite(value) ? value.toPrecision(12) : String(value);
}

function mapSourcePointToTargetPath(point: Vec2, sourcePath: RibbonSample[], targetPath: RibbonSample[]): Vec2 {
  const projection = closestSourcePathProjection(point, sourcePath);
  const sourceA = sourcePath[projection.index];
  const sourceB = sourcePath[projection.index + 1];
  const targetA = targetPath[projection.index];
  const targetB = targetPath[projection.index + 1];
  const targetPoint = {
    x: lerpNumber(targetA.point.x, targetB.point.x, projection.t),
    y: lerpNumber(targetA.point.y, targetB.point.y, projection.t)
  };
  const targetNormal = normalForTangent({
    x: targetB.point.x - targetA.point.x,
    y: targetB.point.y - targetA.point.y
  });
  const sourceRadius = Math.max(0.001, lerpNumber(sourceA.radius, sourceB.radius, projection.t));
  const targetRadius = Math.max(0.001, lerpNumber(targetA.radius, targetB.radius, projection.t));
  const radiusScale = targetRadius / sourceRadius;
  return {
    x: targetPoint.x + targetNormal.x * projection.offset * radiusScale,
    y: targetPoint.y + targetNormal.y * projection.offset * radiusScale
  };
}

function mapSourcePointWithWeightedSkin(point: Vec2, sourcePath: RibbonSample[], targetPath: RibbonSample[], influence: number): Vec2 {
  let totalWeight = 0;
  let mappedX = 0;
  let mappedY = 0;
  let fallback: { point: Vec2; distanceSq: number } | null = null;

  for (let index = 0; index < sourcePath.length - 1; index += 1) {
    const sourceA = sourcePath[index];
    const sourceB = sourcePath[index + 1];
    const targetA = targetPath[index];
    const targetB = targetPath[index + 1];
    const projection = projectPointToSampleSegment(point, sourceA, sourceB);
    if (!projection) continue;
    const mapped = mapPointBySampleSegment(point, sourceA, sourceB, targetA, targetB, projection.unclampedT);
    if (!fallback || projection.distanceSq < fallback.distanceSq) fallback = { point: mapped, distanceSq: projection.distanceSq };

    const sourceRadius = Math.max(0.35, lerpNumber(sourceA.radius, sourceB.radius, projection.t));
    const influenceRadius = Math.max(sourceRadius * influence, 2.4);
    const distance = Math.sqrt(projection.distanceSq);
    const normalizedDistance = distance / influenceRadius;
    const outside = projection.unclampedT < 0 ? -projection.unclampedT : projection.unclampedT > 1 ? projection.unclampedT - 1 : 0;
    const longitudinalPenalty = 1 / (1 + outside * outside * 36);
    const weight = (longitudinalPenalty * longitudinalPenalty) / Math.max(0.0001, 0.035 + normalizedDistance ** 4);
    if (weight <= 0.000001) continue;

    totalWeight += weight;
    mappedX += mapped.x * weight;
    mappedY += mapped.y * weight;
  }

  if (totalWeight <= 0.000001) return fallback?.point ?? point;
  return { x: mappedX / totalWeight, y: mappedY / totalWeight };
}

function projectPointToSampleSegment(point: Vec2, start: RibbonSample, end: RibbonSample) {
  const dx = end.point.x - start.point.x;
  const dy = end.point.y - start.point.y;
  const lengthSq = dx * dx + dy * dy;
  if (lengthSq < 0.000001) return null;
  const unclampedT = ((point.x - start.point.x) * dx + (point.y - start.point.y) * dy) / lengthSq;
  const t = clampNumber(unclampedT, 0, 1);
  const projected = { x: start.point.x + dx * t, y: start.point.y + dy * t };
  return {
    t,
    unclampedT,
    projected,
    distanceSq: squaredDistance(point, projected)
  };
}

function mapPointBySampleSegment(
  point: Vec2,
  sourceA: RibbonSample,
  sourceB: RibbonSample,
  targetA: RibbonSample,
  targetB: RibbonSample,
  t: number
): Vec2 {
  const sourceVector = { x: sourceB.point.x - sourceA.point.x, y: sourceB.point.y - sourceA.point.y };
  const targetVector = { x: targetB.point.x - targetA.point.x, y: targetB.point.y - targetA.point.y };
  const sourceLength = Math.max(0.001, Math.hypot(sourceVector.x, sourceVector.y));
  const targetLength = Math.max(0.001, Math.hypot(targetVector.x, targetVector.y));
  const sourceDirection = { x: sourceVector.x / sourceLength, y: sourceVector.y / sourceLength };
  const targetDirection = { x: targetVector.x / targetLength, y: targetVector.y / targetLength };
  const sourceNormal = normalForTangent(sourceDirection);
  const targetNormal = normalForTangent(targetDirection);
  const relative = { x: point.x - sourceA.point.x, y: point.y - sourceA.point.y };
  const sourceAlong = relative.x * sourceDirection.x + relative.y * sourceDirection.y;
  const sourceOffset = relative.x * sourceNormal.x + relative.y * sourceNormal.y;
  const radiusT = clampNumber(t, 0, 1);
  const sourceRadius = Math.max(0.001, lerpNumber(sourceA.radius, sourceB.radius, radiusT));
  const targetRadius = Math.max(0.001, lerpNumber(targetA.radius, targetB.radius, radiusT));
  const alongScale = targetLength / sourceLength;
  const normalScale = targetRadius / sourceRadius;
  return {
    x: targetA.point.x + targetDirection.x * sourceAlong * alongScale + targetNormal.x * sourceOffset * normalScale,
    y: targetA.point.y + targetDirection.y * sourceAlong * alongScale + targetNormal.y * sourceOffset * normalScale
  };
}

function closestSourcePathProjection(point: Vec2, sourcePath: RibbonSample[]) {
  let best = {
    index: 0,
    t: 0,
    offset: 0,
    distanceSq: Number.POSITIVE_INFINITY
  };

  for (let index = 0; index < sourcePath.length - 1; index += 1) {
    const start = sourcePath[index].point;
    const end = sourcePath[index + 1].point;
    const dx = end.x - start.x;
    const dy = end.y - start.y;
    const lengthSq = dx * dx + dy * dy;
    if (lengthSq < 0.000001) continue;
    const t = clampNumber(((point.x - start.x) * dx + (point.y - start.y) * dy) / lengthSq, 0, 1);
    const projected = { x: start.x + dx * t, y: start.y + dy * t };
    const distanceSq = squaredDistance(point, projected);
    if (distanceSq < best.distanceSq) {
      const normal = normalForTangent({ x: dx, y: dy });
      best = {
        index,
        t,
        offset: (point.x - projected.x) * normal.x + (point.y - projected.y) * normal.y,
        distanceSq
      };
    }
  }

  return best;
}

function sourceTriangleHasAlpha(mask: ImageAlphaMask, a: Vec2, b: Vec2, c: Vec2) {
  const threshold = 8;
  const samples = [
    a,
    b,
    c,
    midpoint(a, b),
    midpoint(b, c),
    midpoint(a, c),
    { x: (a.x + b.x + c.x) / 3, y: (a.y + b.y + c.y) / 3 }
  ];
  return samples.some((point) => alphaAt(mask, point) >= threshold);
}

function squaredDistance(a: Vec2, b: Vec2) {
  const dx = a.x - b.x;
  const dy = a.y - b.y;
  return dx * dx + dy * dy;
}

function clampNumber(value: number, min: number, max: number) {
  return Math.max(min, Math.min(max, value));
}

interface ImageAlphaMask {
  pixelWidth: number;
  pixelHeight: number;
  displayWidth: number;
  displayHeight: number;
  alpha: Uint8ClampedArray;
}

function getImageAlphaMask(image: HTMLImageElement, displayWidth: number, displayHeight: number): ImageAlphaMask | null {
  const cached = alphaMaskCache.get(image);
  if (cached && cached.displayWidth === displayWidth && cached.displayHeight === displayHeight) return cached;
  const pixelWidth = image.naturalWidth;
  const pixelHeight = image.naturalHeight;
  if (pixelWidth <= 0 || pixelHeight <= 0) return null;

  const canvas = document.createElement("canvas");
  canvas.width = pixelWidth;
  canvas.height = pixelHeight;
  const ctx = canvas.getContext("2d", { willReadFrequently: true });
  if (!ctx) return null;
  ctx.clearRect(0, 0, pixelWidth, pixelHeight);
  ctx.drawImage(image, 0, 0, pixelWidth, pixelHeight);
  try {
    const data = ctx.getImageData(0, 0, pixelWidth, pixelHeight).data;
    const alpha = new Uint8ClampedArray(pixelWidth * pixelHeight);
    for (let pixel = 0; pixel < alpha.length; pixel += 1) alpha[pixel] = data[pixel * 4 + 3];
    const mask = { pixelWidth, pixelHeight, displayWidth, displayHeight, alpha };
    alphaMaskCache.set(image, mask);
    return mask;
  } catch {
    return null;
  }
}

function alphaAt(mask: ImageAlphaMask, point: Vec2) {
  const x = Math.round((point.x / mask.displayWidth) * (mask.pixelWidth - 1));
  const y = Math.round((point.y / mask.displayHeight) * (mask.pixelHeight - 1));
  if (x < 0 || y < 0 || x >= mask.pixelWidth || y >= mask.pixelHeight) return 0;
  return mask.alpha[y * mask.pixelWidth + x];
}

function normalForTangent(tangent: Vec2) {
  const length = Math.hypot(tangent.x, tangent.y) || 1;
  return { x: -tangent.y / length, y: tangent.x / length };
}

function traceRibbonPath(ctx: CanvasRenderingContext2D, samples: RibbonSample[], ribbon: RibbonPoint[]) {
  const first = samples[0];
  const last = samples[samples.length - 1];
  const firstRibbon = ribbon[0];
  const lastRibbon = ribbon[ribbon.length - 1];
  ctx.beginPath();
  ctx.moveTo(firstRibbon.left.x, firstRibbon.left.y);
  for (const point of ribbon.slice(1)) ctx.lineTo(point.left.x, point.left.y);
  addRoundCap(ctx, last.point, last.radius, lastRibbon.left, lastRibbon.right);
  for (const point of [...ribbon].reverse().slice(1)) ctx.lineTo(point.right.x, point.right.y);
  addRoundCap(ctx, first.point, first.radius, firstRibbon.right, firstRibbon.left);
  ctx.closePath();
}

function addRoundCap(ctx: CanvasRenderingContext2D, center: Vec2, radius: number, from: Vec2, to: Vec2) {
  const startAngle = Math.atan2(from.y - center.y, from.x - center.x);
  const endAngle = Math.atan2(to.y - center.y, to.x - center.x);
  ctx.arc(center.x, center.y, radius, startAngle, endAngle, true);
}

function ribbonSamples(points: Vec2[], radii: number[], segmentCount = 12): RibbonSample[] {
  if (points.length < 3) {
    return points.map((point, index) => ({ point, tangent: tangentForPolyline(points, index), radius: radii[index] }));
  }

  if (points.length !== 3) {
    return multiJointFilletSamples(points, radii, segmentCount);
  }

  const keypoints: [Vec2, Vec2, Vec2] = [points[0], points[1], points[2]];
  return jointFilletSamples(keypoints, radii, segmentCount);
}

interface RibbonSample {
  point: Vec2;
  tangent: Vec2;
  radius: number;
}

interface RibbonPoint {
  left: Vec2;
  right: Vec2;
}

function jointFilletSamples(points: [Vec2, Vec2, Vec2], radii: number[], segmentCount: number) {
  const [start, joint, end] = points;
  const upperLength = distanceBetween(start, joint);
  const lowerLength = distanceBetween(joint, end);
  if (upperLength < 0.001 || lowerLength < 0.001) {
    return points.map((point, index) => ({ point, tangent: tangentForPolyline(points, index), radius: radii[index] }));
  }

  const upperDirection = normalizeVector({ x: joint.x - start.x, y: joint.y - start.y });
  const lowerDirection = normalizeVector({ x: end.x - joint.x, y: end.y - joint.y });
  const jointRadius = radii[1] ?? Math.max(radii[0] ?? 0, radii[2] ?? 0);
  const filletLength = Math.min(upperLength * 0.22, lowerLength * 0.22, Math.max(4, jointRadius * 0.8));
  const beforeJoint = {
    x: joint.x - upperDirection.x * filletLength,
    y: joint.y - upperDirection.y * filletLength
  };
  const afterJoint = {
    x: joint.x + lowerDirection.x * filletLength,
    y: joint.y + lowerDirection.y * filletLength
  };
  const curveSteps = Math.max(3, Math.round(segmentCount * 0.5));

  const samples = [
    { point: start, tangent: upperDirection, radius: radii[0] },
    { point: midpoint(start, beforeJoint), tangent: upperDirection, radius: averageNumber(radii[0], radii[1]) }
  ];

  for (let index = 0; index <= curveSteps; index += 1) {
    const t = index / curveSteps;
    samples.push({
      point: quadraticPoint(beforeJoint, joint, afterJoint, t),
      tangent: quadraticTangent(beforeJoint, joint, afterJoint, t),
      radius: jointRadius
    });
  }

  samples.push(
    { point: midpoint(afterJoint, end), tangent: lowerDirection, radius: averageNumber(radii[1], radii[2]) },
    { point: end, tangent: lowerDirection, radius: radii[2] }
  );
  return samples;
}

function multiJointFilletSamples(points: Vec2[], radii: number[], segmentCount: number): RibbonSample[] {
  const samples: RibbonSample[] = [];
  const firstDirection = normalizeVector({ x: points[1].x - points[0].x, y: points[1].y - points[0].y });
  samples.push({ point: points[0], tangent: firstDirection, radius: radii[0] });

  for (let index = 1; index < points.length - 1; index += 1) {
    const prev = points[index - 1];
    const joint = points[index];
    const next = points[index + 1];
    const prevLength = distanceBetween(prev, joint);
    const nextLength = distanceBetween(joint, next);
    if (prevLength < 0.001 || nextLength < 0.001) {
      samples.push({ point: joint, tangent: tangentForPolyline(points, index), radius: radii[index] });
      continue;
    }

    const prevDirection = normalizeVector({ x: joint.x - prev.x, y: joint.y - prev.y });
    const nextDirection = normalizeVector({ x: next.x - joint.x, y: next.y - joint.y });
    const jointRadius = radii[index] ?? Math.max(radii[index - 1] ?? 0, radii[index + 1] ?? 0);
    const filletLength = Math.min(prevLength * 0.22, nextLength * 0.22, Math.max(3, jointRadius * 0.75));
    const beforeJoint = {
      x: joint.x - prevDirection.x * filletLength,
      y: joint.y - prevDirection.y * filletLength
    };
    const afterJoint = {
      x: joint.x + nextDirection.x * filletLength,
      y: joint.y + nextDirection.y * filletLength
    };
    const curveSteps = Math.max(3, Math.round(segmentCount * 0.35));

    samples.push({
      point: midpoint(samples[samples.length - 1].point, beforeJoint),
      tangent: prevDirection,
      radius: averageNumber(radii[index - 1], radii[index])
    });

    for (let step = 0; step <= curveSteps; step += 1) {
      const t = step / curveSteps;
      samples.push({
        point: quadraticPoint(beforeJoint, joint, afterJoint, t),
        tangent: quadraticTangent(beforeJoint, joint, afterJoint, t),
        radius: lerpNumber(averageNumber(radii[index - 1], radii[index]), averageNumber(radii[index], radii[index + 1]), t)
      });
    }
  }

  const lastIndex = points.length - 1;
  samples.push({
    point: points[lastIndex],
    tangent: normalizeVector({ x: points[lastIndex].x - points[lastIndex - 1].x, y: points[lastIndex].y - points[lastIndex - 1].y }),
    radius: radii[lastIndex]
  });
  return samples;
}

function ribbonPoint(point: Vec2, tangent: Vec2, radius: number) {
  const length = Math.hypot(tangent.x, tangent.y) || 1;
  const nx = -tangent.y / length;
  const ny = tangent.x / length;
  return {
    left: { x: point.x + nx * radius, y: point.y + ny * radius },
    right: { x: point.x - nx * radius, y: point.y - ny * radius }
  };
}

function tangentForPolyline(points: Vec2[], index: number): Vec2 {
  const previous = points[Math.max(0, index - 1)];
  const next = points[Math.min(points.length - 1, index + 1)];
  return { x: next.x - previous.x, y: next.y - previous.y };
}

function normalizeVector(point: Vec2): Vec2 {
  const length = Math.hypot(point.x, point.y) || 1;
  return { x: point.x / length, y: point.y / length };
}

function distanceBetween(a: Vec2, b: Vec2) {
  return Math.hypot(a.x - b.x, a.y - b.y);
}

function averageNumber(a: number, b: number) {
  return (a + b) * 0.5;
}

function lerpNumber(a: number, b: number, t: number) {
  return a + (b - a) * t;
}

function quadraticPoint(a: Vec2, b: Vec2, c: Vec2, t: number): Vec2 {
  return {
    x: quadraticNumber(a.x, b.x, c.x, t),
    y: quadraticNumber(a.y, b.y, c.y, t)
  };
}

function quadraticTangent(a: Vec2, b: Vec2, c: Vec2, t: number): Vec2 {
  const left = 1 - t;
  return {
    x: 2 * left * (b.x - a.x) + 2 * t * (c.x - b.x),
    y: 2 * left * (b.y - a.y) + 2 * t * (c.y - b.y)
  };
}

function quadraticNumber(a: number, b: number, c: number, t: number) {
  const left = 1 - t;
  return left * left * a + 2 * left * t * b + t * t * c;
}

function drawTexturedTriangle(
  ctx: CanvasRenderingContext2D,
  image: HTMLImageElement,
  width: number,
  height: number,
  s0: Vec2,
  s1: Vec2,
  s2: Vec2,
  d0: Vec2,
  d1: Vec2,
  d2: Vec2,
  sourceAffine?: SourceTriangleAffine
) {
  const matrix = sourceAffine ? affineFromSourceTriangle(sourceAffine, d0, d1, d2) : affineFromTriangles(s0, s1, s2, d0, d1, d2);
  if (!matrix) return;
  const centerX = (d0.x + d1.x + d2.x) / 3;
  const centerY = (d0.y + d1.y + d2.y) / 3;
  const d0dx = d0.x - centerX;
  const d0dy = d0.y - centerY;
  const d0Length = Math.hypot(d0dx, d0dy) || 1;
  const d0x = d0.x + (d0dx / d0Length) * 0.35;
  const d0y = d0.y + (d0dy / d0Length) * 0.35;
  const d1dx = d1.x - centerX;
  const d1dy = d1.y - centerY;
  const d1Length = Math.hypot(d1dx, d1dy) || 1;
  const d1x = d1.x + (d1dx / d1Length) * 0.35;
  const d1y = d1.y + (d1dy / d1Length) * 0.35;
  const d2dx = d2.x - centerX;
  const d2dy = d2.y - centerY;
  const d2Length = Math.hypot(d2dx, d2dy) || 1;
  const d2x = d2.x + (d2dx / d2Length) * 0.35;
  const d2y = d2.y + (d2dy / d2Length) * 0.35;
  ctx.save();
  ctx.beginPath();
  ctx.moveTo(d0x, d0y);
  ctx.lineTo(d1x, d1y);
  ctx.lineTo(d2x, d2y);
  ctx.closePath();
  ctx.clip();
  ctx.transform(matrix.a, matrix.b, matrix.c, matrix.d, matrix.e, matrix.f);
  ctx.drawImage(image, 0, 0, width, height);
  ctx.restore();
}

export function affineFromTriangles(s0: Vec2, s1: Vec2, s2: Vec2, d0: Vec2, d1: Vec2, d2: Vec2): Mat2D | null {
  const det = s0.x * (s1.y - s2.y) + s1.x * (s2.y - s0.y) + s2.x * (s0.y - s1.y);
  if (Math.abs(det) < 1e-6) return null;
  return {
    a: (d0.x * (s1.y - s2.y) + d1.x * (s2.y - s0.y) + d2.x * (s0.y - s1.y)) / det,
    b: (d0.y * (s1.y - s2.y) + d1.y * (s2.y - s0.y) + d2.y * (s0.y - s1.y)) / det,
    c: (d0.x * (s2.x - s1.x) + d1.x * (s0.x - s2.x) + d2.x * (s1.x - s0.x)) / det,
    d: (d0.y * (s2.x - s1.x) + d1.y * (s0.x - s2.x) + d2.y * (s1.x - s0.x)) / det,
    e:
      (d0.x * (s1.x * s2.y - s2.x * s1.y) +
        d1.x * (s2.x * s0.y - s0.x * s2.y) +
        d2.x * (s0.x * s1.y - s1.x * s0.y)) /
      det,
    f:
      (d0.y * (s1.x * s2.y - s2.x * s1.y) +
        d1.y * (s2.x * s0.y - s0.x * s2.y) +
        d2.y * (s0.x * s1.y - s1.x * s0.y)) /
      det
  };
}

function sourceTriangleAffine(s0: Vec2, s1: Vec2, s2: Vec2): SourceTriangleAffine {
  const det = s0.x * (s1.y - s2.y) + s1.x * (s2.y - s0.y) + s2.x * (s0.y - s1.y);
  const invDet = Math.abs(det) < 1e-6 ? 0 : 1 / det;
  return {
    invDet,
    a0: s1.y - s2.y,
    a1: s2.y - s0.y,
    a2: s0.y - s1.y,
    c0: s2.x - s1.x,
    c1: s0.x - s2.x,
    c2: s1.x - s0.x,
    e0: s1.x * s2.y - s2.x * s1.y,
    e1: s2.x * s0.y - s0.x * s2.y,
    e2: s0.x * s1.y - s1.x * s0.y
  };
}

function affineFromSourceTriangle(source: SourceTriangleAffine, d0: Vec2, d1: Vec2, d2: Vec2): Mat2D | null {
  if (source.invDet === 0) return null;
  return {
    a: (d0.x * source.a0 + d1.x * source.a1 + d2.x * source.a2) * source.invDet,
    b: (d0.y * source.a0 + d1.y * source.a1 + d2.y * source.a2) * source.invDet,
    c: (d0.x * source.c0 + d1.x * source.c1 + d2.x * source.c2) * source.invDet,
    d: (d0.y * source.c0 + d1.y * source.c1 + d2.y * source.c2) * source.invDet,
    e: (d0.x * source.e0 + d1.x * source.e1 + d2.x * source.e2) * source.invDet,
    f: (d0.y * source.e0 + d1.y * source.e1 + d2.y * source.e2) * source.invDet
  };
}

function drawBindingLinks(ctx: CanvasRenderingContext2D, rig: ResolvedRig, viewport: RigViewport, options: SkeletalRenderOptions) {
  ctx.save();
  ctx.lineWidth = 1 / viewport.scale;
  ctx.font = `${11 / viewport.scale}px ui-monospace, SFMono-Regular, Menlo, monospace`;
  for (const binding of rig.bindings) {
    const selected = binding.id === options.selectedBindingId;
    ctx.strokeStyle = selected ? "rgba(200, 167, 90, 0.95)" : "rgba(200, 167, 90, 0.34)";
    ctx.beginPath();
    ctx.moveTo(binding.anchor.position.x, binding.anchor.position.y);
    ctx.lineTo(binding.position.x, binding.position.y);
    ctx.stroke();
    if (options.showLabels && selected) {
      drawLabel(ctx, midpoint(binding.anchor.position, binding.position), binding.definition.anchorId, "#9b9a8f", viewport.scale);
    }
  }
  ctx.restore();
}

function drawSkeletonGuides(ctx: CanvasRenderingContext2D, rig: ResolvedRig, viewport: RigViewport, options: SkeletalRenderOptions) {
  const deformBindings = rig.bindings.filter(
    (binding) => binding.definition.kind === "mesh" && binding.definition.opacity > 0.001 && binding.definition.deform?.keypoints.length
  );
  if (deformBindings.length > 0) {
    drawDeformChains(ctx, rig, deformBindings, viewport, options);
    return;
  }
  drawBones(ctx, rig, viewport, options);
}

function drawDeformChains(
  ctx: CanvasRenderingContext2D,
  rig: ResolvedRig,
  bindings: ResolvedBinding[],
  viewport: RigViewport,
  options: SkeletalRenderOptions
) {
  ctx.save();
  ctx.lineCap = "round";
  ctx.lineJoin = "round";
  for (const binding of bindings) {
    const keypoints = binding.definition.deform?.keypoints || [];
    const anchors = keypoints.map((keypoint) => rig.anchorsById[keypoint.anchorId]).filter((anchor) => !!anchor);
    if (anchors.length < 2) continue;
    const selected = binding.id === options.selectedBindingId || anchors.some((anchor) => anchor.id === options.selectedAnchorId);
    const color = selected ? "#54c6b1" : anchors[0]?.definition.color || "rgba(84, 198, 177, 0.72)";
    ctx.strokeStyle = color;
    ctx.lineWidth = (selected ? 3 : 2) / viewport.scale;
    ctx.beginPath();
    ctx.moveTo(anchors[0].position.x, anchors[0].position.y);
    for (const anchor of anchors.slice(1)) ctx.lineTo(anchor.position.x, anchor.position.y);
    ctx.stroke();
    if (!options.showAnchors) {
      ctx.fillStyle = "#101211";
      ctx.strokeStyle = color;
      ctx.lineWidth = 1 / viewport.scale;
      for (const anchor of anchors) {
        ctx.beginPath();
        ctx.arc(anchor.position.x, anchor.position.y, (selected ? 4 : 3.2) / viewport.scale, 0, Math.PI * 2);
        ctx.fill();
        ctx.stroke();
      }
    }
    if (options.showLabels && selected) drawLabel(ctx, anchors[Math.floor(anchors.length * 0.5)].position, binding.name, color, viewport.scale);
  }
  ctx.restore();
}

function drawBones(ctx: CanvasRenderingContext2D, rig: ResolvedRig, viewport: RigViewport, options: SkeletalRenderOptions) {
  ctx.save();
  ctx.lineCap = "round";
  ctx.lineJoin = "round";
  for (const bone of rig.bones) {
    const selected = bone.id === options.selectedBoneId;
    ctx.strokeStyle = selected ? "#54c6b1" : bone.definition.color || "rgba(84, 198, 177, 0.72)";
    ctx.lineWidth = (selected ? 3 : 2) / viewport.scale;
    ctx.beginPath();
    ctx.moveTo(bone.start.x, bone.start.y);
    ctx.lineTo(bone.end.x, bone.end.y);
    ctx.stroke();
    ctx.fillStyle = selected ? "#54c6b1" : "#101211";
    ctx.strokeStyle = selected ? "#e8e1cf" : "rgba(84, 198, 177, 0.72)";
    ctx.lineWidth = 1 / viewport.scale;
    ctx.beginPath();
    ctx.arc(bone.start.x, bone.start.y, 3.4 / viewport.scale, 0, Math.PI * 2);
    ctx.fill();
    ctx.stroke();
    if (bone.definition.length > 0) {
      ctx.beginPath();
      ctx.arc(bone.end.x, bone.end.y, 4 / viewport.scale, 0, Math.PI * 2);
      ctx.fill();
      ctx.stroke();
    }
    if (options.showLabels && selected) drawLabel(ctx, bone.end, bone.name, "#54c6b1", viewport.scale);
  }
  ctx.restore();
}

function drawAnchors(ctx: CanvasRenderingContext2D, rig: ResolvedRig, viewport: RigViewport, options: SkeletalRenderOptions) {
  ctx.save();
  ctx.textBaseline = "middle";
  for (const anchor of rig.anchors) {
    const selected = anchor.id === options.selectedAnchorId;
    const color = selected ? "#c8a75a" : anchor.definition.color || "#d35d56";
    ctx.fillStyle = color;
    ctx.strokeStyle = "rgba(0, 0, 0, 0.72)";
    ctx.lineWidth = 1.4 / viewport.scale;
    ctx.beginPath();
    ctx.arc(anchor.position.x, anchor.position.y, (selected ? 5 : 3.5) / viewport.scale, 0, Math.PI * 2);
    ctx.fill();
    ctx.stroke();
    if (options.showLabels && (selected || anchor.bindingIds.length > 0)) {
      drawLabel(ctx, { x: anchor.position.x + 5 / viewport.scale, y: anchor.position.y - 7 / viewport.scale }, anchor.id, color, viewport.scale);
    }
  }
  ctx.restore();
}

function drawLabel(ctx: CanvasRenderingContext2D, point: Vec2, text: string, color: string, scale: number) {
  ctx.save();
  ctx.font = `${11 / scale}px ui-monospace, SFMono-Regular, Menlo, monospace`;
  const metrics = ctx.measureText(text);
  const x = point.x;
  const y = point.y;
  ctx.fillStyle = "rgba(0, 0, 0, 0.72)";
  ctx.fillRect(x - 3 / scale, y - 12 / scale, metrics.width + 7 / scale, 15 / scale);
  ctx.fillStyle = color;
  ctx.fillText(text, x, y);
  ctx.restore();
}

function drawCapsule(
  ctx: CanvasRenderingContext2D,
  width: number,
  height: number,
  fill: string,
  stroke: string | undefined,
  viewportScale: number
) {
  const radius = height * 0.5;
  ctx.beginPath();
  ctx.moveTo(radius, -radius);
  ctx.lineTo(Math.max(radius, width - radius), -radius);
  ctx.arc(Math.max(radius, width - radius), 0, radius, -Math.PI / 2, Math.PI / 2);
  ctx.lineTo(radius, radius);
  ctx.arc(radius, 0, radius, Math.PI / 2, -Math.PI / 2);
  ctx.closePath();
  ctx.fillStyle = fill;
  ctx.fill();
  if (stroke) {
    ctx.strokeStyle = stroke;
    ctx.lineWidth = 1 / viewportScale;
    ctx.stroke();
  }
}

function applyMatrix(ctx: CanvasRenderingContext2D, matrix: Mat2D) {
  ctx.transform(matrix.a, matrix.b, matrix.c, matrix.d, matrix.e, matrix.f);
}

function midpoint(a: Vec2, b: Vec2): Vec2 {
  return { x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 };
}
