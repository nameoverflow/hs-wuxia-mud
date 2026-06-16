import { screenToRig } from "./math";
import type { Mat2D, ResolvedBinding, ResolvedRig, RigViewport, SkeletalRenderOptions, Vec2 } from "./types";

const defaultOptions: SkeletalRenderOptions = {
  showBones: true,
  showAnchors: true,
  showBindings: true,
  showImages: true,
  showSkin: true,
  showLabels: true,
  zoom: 1,
  background: "#070808"
};

const alphaMaskCache = new WeakMap<HTMLImageElement, ImageAlphaMask>();

export class SkeletalCanvasRenderer {
  private images = new Map<string, HTMLImageElement>();
  private onAssetLoad: () => void;

  constructor(onAssetLoad: () => void = () => undefined) {
    this.onAssetLoad = onAssetLoad;
  }

  render(ctx: CanvasRenderingContext2D, rig: ResolvedRig, options: Partial<SkeletalRenderOptions> = {}): RigViewport {
    const merged = { ...defaultOptions, ...options };
    const viewport = fitRigViewport(ctx.canvas.width, ctx.canvas.height, rig.definition.canvas, merged.zoom ?? 1);
    viewport.offsetX += merged.panX ?? 0;
    viewport.offsetY += merged.panY ?? 0;
    ctx.save();
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
    ctx.fillStyle = merged.background || "#070808";
    ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
    drawGrid(ctx, viewport, rig.definition.canvas);
    ctx.setTransform(viewport.scale, 0, 0, viewport.scale, viewport.offsetX, viewport.offsetY);
    drawStage(ctx, rig, viewport);

    for (const binding of rig.bindings) {
      if ((binding.definition.kind === "image" || binding.definition.kind === "mesh") && !merged.showImages) continue;
      if (binding.definition.tags?.includes("skin") && !merged.showSkin) continue;
      drawBinding(ctx, rig, binding, viewport, merged, this.loadImage(binding.definition.image));
    }

    if (merged.showBindings) drawBindingLinks(ctx, rig, viewport, merged);
    if (merged.showBones) drawBones(ctx, rig, viewport, merged);
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
    drawMeshBinding(ctx, rig, binding, viewport, image);
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
    const alphaMesh = buildAlphaContourMesh(image, def.width, def.height, sourceCenters, targetCenters, sourceRadii, radii, deform.segments);
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

interface TexturedGridMesh {
  sourceRows: Vec2[][];
  targetRows: Vec2[][];
}

function drawTexturedGridTriangles(ctx: CanvasRenderingContext2D, image: HTMLImageElement, width: number, height: number, mesh: TexturedGridMesh) {
  const rowCount = Math.min(mesh.sourceRows.length, mesh.targetRows.length);
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
      drawTexturedTriangle(ctx, image, width, height, s0, s1, s2, d0, d1, d2);
      drawTexturedTriangle(ctx, image, width, height, s2, s1, s3, d2, d1, d3);
    }
  }
}

function buildAlphaContourMesh(
  image: HTMLImageElement,
  width: number,
  height: number,
  sourceCenters: Vec2[],
  targetCenters: Vec2[],
  sourceRadii: number[],
  targetRadii: number[],
  segmentCount = 12
): TexturedGridMesh | null {
  const mask = getImageAlphaMask(image, width, height);
  if (!mask || sourceCenters.length !== 3 || targetCenters.length !== 3) return null;
  const rows = pairedContourSamples(mask, sourceCenters, targetCenters, sourceRadii, targetRadii, segmentCount);
  if (rows.length < 2) return null;

  const sourceRows: Vec2[][] = [];
  const targetRows: Vec2[][] = [];
  const columnCount = 7;
  for (const row of rows) {
    const sourceNormal = normalForTangent(row.source.tangent);
    const targetNormal = normalForTangent(row.target.tangent);
    const span = alphaSpanAlongNormal(mask, row.source.point, sourceNormal) || {
      start: -row.source.radius,
      end: row.source.radius
    };
    sourceRows.push(sampleMeshRow(row.source.point, sourceNormal, span.start, span.end, columnCount));
    targetRows.push(sampleMeshRow(row.target.point, targetNormal, span.start, span.end, columnCount));
  }

  return { sourceRows, targetRows };
}

function sampleMeshRow(center: Vec2, normal: Vec2, start: number, end: number, columnCount: number) {
  const row: Vec2[] = [];
  for (let column = 0; column < columnCount; column += 1) {
    const t = column / Math.max(1, columnCount - 1);
    const offset = start + (end - start) * t;
    row.push({ x: center.x + normal.x * offset, y: center.y + normal.y * offset });
  }
  return row;
}

function pairedContourSamples(
  mask: ImageAlphaMask,
  sourceCenters: Vec2[],
  targetCenters: Vec2[],
  sourceRadii: number[],
  targetRadii: number[],
  segmentCount: number
) {
  const sourceSamples = ribbonSamples(sourceCenters, sourceRadii, segmentCount);
  const targetSamples = ribbonSamples(targetCenters, targetRadii, segmentCount);
  const samples = sourceSamples.map((source, index) => ({ source, target: targetSamples[index] })).filter((row) => row.target);
  const sourceStartDirection = normalizeVector({ x: sourceCenters[1].x - sourceCenters[0].x, y: sourceCenters[1].y - sourceCenters[0].y });
  const sourceEndDirection = normalizeVector({ x: sourceCenters[2].x - sourceCenters[1].x, y: sourceCenters[2].y - sourceCenters[1].y });
  const targetStartDirection = normalizeVector({ x: targetCenters[1].x - targetCenters[0].x, y: targetCenters[1].y - targetCenters[0].y });
  const targetEndDirection = normalizeVector({ x: targetCenters[2].x - targetCenters[1].x, y: targetCenters[2].y - targetCenters[1].y });
  const startScale = safeRatio(distanceBetween(targetCenters[0], targetCenters[1]), distanceBetween(sourceCenters[0], sourceCenters[1]));
  const endScale = safeRatio(distanceBetween(targetCenters[2], targetCenters[1]), distanceBetween(sourceCenters[2], sourceCenters[1]));
  const startExtension = alphaExtensionAlongTangent(mask, sourceCenters[0], sourceStartDirection, -1, sourceRadii[0]);
  const endExtension = alphaExtensionAlongTangent(mask, sourceCenters[2], sourceEndDirection, 1, sourceRadii[2]);

  return [
    ...extensionSamples(sourceCenters[0], targetCenters[0], sourceStartDirection, targetStartDirection, sourceRadii[0], targetRadii[0], startExtension, startScale, -1, true),
    ...samples,
    ...extensionSamples(sourceCenters[2], targetCenters[2], sourceEndDirection, targetEndDirection, sourceRadii[2], targetRadii[2], endExtension, endScale, 1, false)
  ];
}

function extensionSamples(
  sourceCenter: Vec2,
  targetCenter: Vec2,
  sourceDirection: Vec2,
  targetDirection: Vec2,
  sourceRadius: number,
  targetRadius: number,
  extension: number,
  targetScale: number,
  sign: -1 | 1,
  reverse: boolean
) {
  if (extension < 0.75) return [];
  const count = Math.min(5, Math.max(2, Math.ceil(extension / 3)));
  const rows = Array.from({ length: count }, (_, index) => {
    const t = (index + 1) / count;
    return {
      source: {
        point: {
          x: sourceCenter.x + sourceDirection.x * sign * extension * t,
          y: sourceCenter.y + sourceDirection.y * sign * extension * t
        },
        tangent: sourceDirection,
        radius: sourceRadius
      },
      target: {
        point: {
          x: targetCenter.x + targetDirection.x * sign * extension * targetScale * t,
          y: targetCenter.y + targetDirection.y * sign * extension * targetScale * t
        },
        tangent: targetDirection,
        radius: targetRadius
      }
    };
  });
  return reverse ? rows.reverse() : rows;
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

function alphaSpanAlongNormal(mask: ImageAlphaMask, center: Vec2, normal: Vec2): { start: number; end: number } | null {
  const step = 0.45;
  const threshold = 12;
  const maxDistance = Math.hypot(mask.displayWidth, mask.displayHeight);
  const spans: { start: number; end: number }[] = [];
  let inSpan = false;
  let spanStart = -maxDistance;
  let lastInside = -maxDistance;

  for (let offset = -maxDistance; offset <= maxDistance; offset += step) {
    const inside = alphaAt(mask, { x: center.x + normal.x * offset, y: center.y + normal.y * offset }) >= threshold;
    if (inside) {
      if (!inSpan) spanStart = offset;
      inSpan = true;
      lastInside = offset;
    } else if (inSpan) {
      spans.push({ start: spanStart, end: lastInside });
      inSpan = false;
    }
  }
  if (inSpan) spans.push({ start: spanStart, end: lastInside });
  if (spans.length === 0) return null;

  return spans
    .filter((span) => span.end - span.start >= 0.25)
    .sort((a, b) => distanceToSpan(0, a) - distanceToSpan(0, b) || b.end - b.start - (a.end - a.start))[0] ?? null;
}

function alphaExtensionAlongTangent(mask: ImageAlphaMask, center: Vec2, direction: Vec2, sign: -1 | 1, radius: number) {
  const step = 0.45;
  const threshold = 12;
  const maxDistance = Math.min(Math.hypot(mask.displayWidth, mask.displayHeight), Math.max(5, radius * 2.4));
  let sawInside = alphaAt(mask, center) >= threshold;
  let lastInside = sawInside ? 0 : -1;
  for (let distance = step; distance <= maxDistance; distance += step) {
    const point = {
      x: center.x + direction.x * sign * distance,
      y: center.y + direction.y * sign * distance
    };
    const inside = alphaAt(mask, point) >= threshold;
    if (inside) {
      sawInside = true;
      lastInside = distance;
    } else if (sawInside) {
      break;
    }
  }
  return Math.max(0, lastInside);
}

function alphaAt(mask: ImageAlphaMask, point: Vec2) {
  const x = Math.round((point.x / mask.displayWidth) * (mask.pixelWidth - 1));
  const y = Math.round((point.y / mask.displayHeight) * (mask.pixelHeight - 1));
  if (x < 0 || y < 0 || x >= mask.pixelWidth || y >= mask.pixelHeight) return 0;
  return mask.alpha[y * mask.pixelWidth + x];
}

function distanceToSpan(value: number, span: { start: number; end: number }) {
  if (value >= span.start && value <= span.end) return 0;
  return Math.min(Math.abs(value - span.start), Math.abs(value - span.end));
}

function normalForTangent(tangent: Vec2) {
  const length = Math.hypot(tangent.x, tangent.y) || 1;
  return { x: -tangent.y / length, y: tangent.x / length };
}

function safeRatio(numerator: number, denominator: number) {
  if (Math.abs(denominator) < 0.001) return 1;
  return numerator / denominator;
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
  if (points.length !== 3) {
    return points.map((point, index) => ({ point, tangent: tangentForPolyline(points, index), radius: radii[index] }));
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
  d2: Vec2
) {
  const matrix = affineFromTriangles(s0, s1, s2, d0, d1, d2);
  if (!matrix) return;
  ctx.save();
  ctx.beginPath();
  ctx.moveTo(d0.x, d0.y);
  ctx.lineTo(d1.x, d1.y);
  ctx.lineTo(d2.x, d2.y);
  ctx.closePath();
  ctx.clip();
  ctx.transform(matrix.a, matrix.b, matrix.c, matrix.d, matrix.e, matrix.f);
  ctx.drawImage(image, 0, 0, width, height);
  ctx.restore();
}

function affineFromTriangles(s0: Vec2, s1: Vec2, s2: Vec2, d0: Vec2, d1: Vec2, d2: Vec2): Mat2D | null {
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
