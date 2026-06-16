#!/usr/bin/env node
import { createCanvas, Image } from "@napi-rs/canvas";
import { createServer } from "vite";
import { mkdirSync, writeFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const clientRoot = path.resolve(scriptDir, "..");
const repoRoot = path.resolve(clientRoot, "..");
const reviewVariants = [
  { id: "debug", label: "black + guides" },
  { id: "clean-black", label: "black clean" },
  { id: "clean-transparent", label: "transparent clean" }
];

globalThis.Image = Image;
globalThis.document = {
  createElement(tagName) {
    if (tagName !== "canvas") throw new Error(`Unsupported render element: ${tagName}`);
    return createCanvas(1, 1);
  }
};

const options = parseArgs(process.argv.slice(2));

const vite = await createServer({
  root: clientRoot,
  logLevel: "error",
  server: { middlewareMode: true, hmr: false },
  appType: "custom"
});

try {
  const [{ createBattleActorRig, skeletalAnimationEntries }, { resolveRig }, rendererModule] = await Promise.all([
    vite.ssrLoadModule("/src/battle/skeletal/catalog.ts"),
    vite.ssrLoadModule("/src/battle/skeletal/runtime.ts"),
    vite.ssrLoadModule("/src/battle/skeletal/renderer.ts")
  ]);
  const { buildSkeletonWarpMesh, fitRigViewport, SkeletalCanvasRenderer } = rendererModule;

  const items = selectRenderItems(skeletalAnimationEntries, options);
  const renderContext = { createBattleActorRig, resolveRig, buildSkeletonWarpMesh, fitRigViewport, SkeletalCanvasRenderer };
  const result =
    options.mode === "review"
      ? await renderReviewSheet(items, options, renderContext)
      : await renderSingle(items[0], options, renderContext, modeToVariant(options.mode));

  mkdirSync(path.dirname(options.out), { recursive: true });
  writeFileSync(options.out, result.canvas.toBuffer("image/png"));
  console.log(
    JSON.stringify(
      {
        out: path.relative(repoRoot, options.out),
        items: result.items,
        mode: options.mode,
        size: [result.canvas.width, result.canvas.height],
        panelSize: [options.width, options.height],
        fit: options.fit,
        ...(result.view ? { zoom: result.view.zoom, pan: [result.view.panX, result.view.panY] } : {})
      },
      null,
      2
    )
  );
} finally {
  await vite.close();
}

function parseArgs(args) {
  const values = {
    entry: "",
    entries: [],
    actions: [],
    poses: [],
    profile: "female",
    style: "fist",
    tag: "v12",
    pose: "",
    mode: "debug",
    fit: "actor",
    out: path.resolve(repoRoot, "harness/tmp/rig-preview.png"),
    width: 960,
    height: 720,
    zoom: null,
    panX: 0,
    panY: 0,
    selectedBone: "frontArm",
    selectedAnchor: "frontWrist",
    selectedBinding: "part.arm_front"
  };

  for (let index = 0; index < args.length; index += 1) {
    const arg = args[index];
    if (arg === "--help" || arg === "-h") {
      printHelp();
      process.exit(0);
    }
    if (!arg.startsWith("--")) throw new Error(`Unexpected argument: ${arg}`);
    const key = arg.slice(2);
    const value = args[index + 1];
    if (value === undefined || value.startsWith("--")) throw new Error(`Missing value for ${arg}`);
    index += 1;
    switch (key) {
      case "entry":
        values.entry = value;
        values.entries.push(value);
        break;
      case "entries":
      case "actions":
      case "poses":
        values[toCamelCase(key)].push(...splitList(value));
        break;
      case "profile":
      case "style":
      case "tag":
      case "pose":
      case "mode":
      case "fit":
      case "selected-bone":
      case "selected-anchor":
      case "selected-binding":
        values[toCamelCase(key)] = value;
        break;
      case "out":
        values.out = path.resolve(repoRoot, value);
        break;
      case "width":
      case "height":
      case "zoom":
      case "pan-x":
      case "pan-y":
        values[toCamelCase(key)] = Number(value);
        if (!Number.isFinite(values[toCamelCase(key)])) throw new Error(`Invalid number for ${arg}: ${value}`);
        break;
      default:
        throw new Error(`Unknown option: ${arg}`);
    }
  }

  if (values.pose && values.poses.length === 0) values.poses.push(values.pose);
  if (values.mode !== "debug" && values.mode !== "clean" && values.mode !== "review") throw new Error("--mode must be debug, clean, or review");
  if (values.fit !== "actor" && values.fit !== "stage") throw new Error("--fit must be actor or stage");
  return values;
}

function toCamelCase(value) {
  return value.replace(/-([a-z])/g, (_, letter) => letter.toUpperCase());
}

function splitList(value) {
  return value
    .split(",")
    .map((item) => item.trim())
    .filter(Boolean);
}

function printHelp() {
  console.log(`Usage:
  npm run render:rig -- [options]

Options:
  --entry <id>               Exact animation entry id, for example segmented.v12.female
  --entries <ids>            Comma-separated entry ids for review sheets
  --actions <ids>            Comma-separated action ids, for example thrust,chop,dodge,parry
  --poses <ids>              Comma-separated pose ids for one selected entry
  --profile <female|male>    Entry profile when --entry is omitted
  --style <fist|sword>       Entry style when --entry is omitted
  --tag <tag>                Required entry tag when --entry is omitted, default v12
  --pose <id>                Pose id, defaults to the entry pose
  --mode <debug|clean|review>
                              review outputs black+guides, black clean, transparent clean in one sheet
  --fit <actor|stage>        Auto-fit actor bounds or rig stage, default actor
  --out <path>               PNG output path, default harness/tmp/rig-preview.png
  --width <px>               Output width, or review panel width, default 960
  --height <px>              Output height, or review panel height, default 720
  --zoom <number>            Manual rig viewport zoom; omitted means auto-fit
  --pan-x <px>               Viewport pan X in output pixels
  --pan-y <px>               Viewport pan Y in output pixels
`);
}

function selectRenderItems(entries, values) {
  const selectedEntries = selectedEntriesForOptions(entries, values);
  const poseIds = values.poses.length > 0 ? values.poses : [];
  return selectedEntries.flatMap((entry) => {
    const ids = poseIds.length > 0 ? poseIds : [entry.poseId];
    return ids.map((poseId) => ({ entry, poseId }));
  });
}

function selectedEntriesForOptions(entries, values) {
  if (values.entries.length > 0) {
    return values.entries.map((id) => {
      const exact = entries.find((entry) => entry.id === id);
      if (!exact) throw new Error(`Unknown animation entry: ${id}`);
      return exact;
    });
  }
  if (values.actions.length > 0) {
    return values.actions.map((action) => {
      const normalized = action.toLowerCase();
      const match = entries.find(
        (entry) =>
          entry.profile === values.profile &&
          entry.style === values.style &&
          (!values.tag || entry.tags.includes(values.tag)) &&
          [entry.id, entry.actionId, entry.poseId, ...entry.tags].some((part) => part.toLowerCase().includes(normalized))
      );
      if (!match) throw new Error(`No action entry found for action=${action}, profile=${values.profile}, style=${values.style}, tag=${values.tag}`);
      return match;
    });
  }
  const match = entries.find(
    (entry) =>
      entry.profile === values.profile &&
      entry.style === values.style &&
      (!values.tag || entry.tags.includes(values.tag)) &&
      (!values.pose || entry.poseId === values.pose || Boolean(entry.tags.includes(values.pose)))
  );
  if (!match) throw new Error(`No entry found for profile=${values.profile}, style=${values.style}, tag=${values.tag}`);
  return [match];
}

function modeToVariant(mode) {
  return mode === "debug" ? "debug" : "clean-transparent";
}

async function renderSingle(item, values, context, variant) {
  const rendered = await renderPanel(item, values, context, variant);
  return {
    canvas: rendered.canvas,
    view: rendered.view,
    items: [{ entry: rendered.entry.id, pose: rendered.pose.id, variant }]
  };
}

async function renderReviewSheet(items, values, context) {
  const gap = 18;
  const titleHeight = 32;
  const rowLabelHeight = 26;
  const footer = 18;
  const panelWidth = values.width;
  const panelHeight = values.height;
  const sheetWidth = gap + reviewVariants.length * (panelWidth + gap);
  const sheetHeight = gap + titleHeight + items.length * (rowLabelHeight + panelHeight + gap) + footer;
  const sheet = createCanvas(sheetWidth, sheetHeight);
  const ctx = sheet.getContext("2d");
  ctx.fillStyle = "#101211";
  ctx.fillRect(0, 0, sheetWidth, sheetHeight);
  ctx.font = "16px ui-monospace, SFMono-Regular, Menlo, monospace";
  ctx.textBaseline = "middle";

  for (let column = 0; column < reviewVariants.length; column += 1) {
    const x = gap + column * (panelWidth + gap);
    drawText(ctx, reviewVariants[column].label, x, gap + titleHeight * 0.5, "#d9d0b4", "16px ui-monospace, SFMono-Regular, Menlo, monospace");
  }

  const renderedItems = [];
  for (let row = 0; row < items.length; row += 1) {
    const rowTop = gap + titleHeight + row * (rowLabelHeight + panelHeight + gap);
    const label = `${items[row].entry.id} / ${items[row].poseId}`;
    drawText(ctx, label, gap, rowTop + rowLabelHeight * 0.5, "#9fb8ad", "14px ui-monospace, SFMono-Regular, Menlo, monospace");
    const commonView = fitItemView(items[row], values, context);

    for (let column = 0; column < reviewVariants.length; column += 1) {
      const variant = reviewVariants[column].id;
      const x = gap + column * (panelWidth + gap);
      const y = rowTop + rowLabelHeight;
      if (variant === "clean-transparent") drawCheckerboard(ctx, x, y, panelWidth, panelHeight);
      const panel = await renderPanel(items[row], values, context, variant, commonView);
      ctx.drawImage(panel.canvas, x, y);
      ctx.strokeStyle = "rgba(232, 225, 207, 0.24)";
      ctx.lineWidth = 1;
      ctx.strokeRect(x + 0.5, y + 0.5, panelWidth - 1, panelHeight - 1);
      renderedItems.push({ entry: panel.entry.id, pose: panel.pose.id, variant, zoom: panel.view.zoom });
    }
  }

  return { canvas: sheet, items: renderedItems };
}

function fitItemView(item, values, context) {
  const rig = prepareRig(context.createBattleActorRig(item.entry), "clean-black");
  const pose = rig.poses[item.poseId] || rig.poses[item.entry.poseId] || rig.poses.idle || Object.values(rig.poses)[0];
  if (!pose) throw new Error(`No pose available for ${item.entry.id}`);
  const resolvedRig = context.resolveRig(rig, pose, { bones: {}, anchors: {}, bindings: {} });
  return fitView({ ...values, mode: "clean" }, resolvedRig, context.buildSkeletonWarpMesh, context.fitRigViewport);
}

async function renderPanel(item, values, context, variant, forcedView = null) {
  const rig = prepareRig(context.createBattleActorRig(item.entry), variant);
  const pose = rig.poses[item.poseId] || rig.poses[item.entry.poseId] || rig.poses.idle || Object.values(rig.poses)[0];
  if (!pose) throw new Error(`No pose available for ${item.entry.id}`);

  const canvas = createCanvas(values.width, values.height);
  const ctx = canvas.getContext("2d");
  const renderer = new context.SkeletalCanvasRenderer();
  await renderer.preloadImages(rig.bindings.map((binding) => binding.image));
  const resolvedRig = context.resolveRig(rig, pose, { bones: {}, anchors: {}, bindings: {} });
  const view = forcedView || fitView({ ...values, mode: variant === "debug" ? "debug" : "clean" }, resolvedRig, context.buildSkeletonWarpMesh, context.fitRigViewport);
  renderer.render(ctx, resolvedRig, renderOptions(values, view, variant));
  return { canvas, view, entry: item.entry, pose };
}

function prepareRig(rig, variant) {
  return {
    ...rig,
    bindings: rig.bindings
      .filter((binding) => variant === "debug" || !binding.id.startsWith("target."))
      .map((binding) => ({
        ...binding,
        ...(binding.image ? { image: toFilesystemImagePath(binding.image) } : {})
      }))
  };
}

function toFilesystemImagePath(image) {
  if (image.startsWith("/src/")) return path.join(clientRoot, image.slice(1));
  if (image.startsWith("/")) return image;
  return path.resolve(clientRoot, image);
}

function fitView(values, resolvedRig, buildSkeletonWarpMesh, fitRigViewport) {
  if (values.zoom !== null || values.fit === "stage") {
    return { zoom: values.zoom ?? 1, panX: values.panX, panY: values.panY };
  }

  const bounds = actorBounds(resolvedRig, buildSkeletonWarpMesh);
  if (!bounds) return { zoom: 1, panX: values.panX, panY: values.panY };

  const padding = values.mode === "debug" ? 54 : 34;
  const baseViewport = fitRigViewport(values.width, values.height, resolvedRig.definition.canvas, 1);
  const targetScale = Math.min(
    (values.width - padding * 2) / Math.max(1, bounds.maxX - bounds.minX),
    (values.height - padding * 2) / Math.max(1, bounds.maxY - bounds.minY)
  );
  const zoom = targetScale / baseViewport.scale;
  const viewport = fitRigViewport(values.width, values.height, resolvedRig.definition.canvas, zoom);
  const centerX = (bounds.minX + bounds.maxX) * 0.5;
  const centerY = (bounds.minY + bounds.maxY) * 0.5;
  return {
    zoom,
    panX: values.width * 0.5 - (centerX * viewport.scale + viewport.offsetX) + values.panX,
    panY: values.height * 0.5 - (centerY * viewport.scale + viewport.offsetY) + values.panY
  };
}

function actorBounds(resolvedRig, buildSkeletonWarpMesh) {
  const points = [];
  for (const binding of resolvedRig.bindings) {
    const def = binding.definition;
    if (def.opacity <= 0.001) continue;
    if (def.id.startsWith("target.")) continue;
    if (def.tags?.includes("source")) continue;
    if (def.kind === "mesh" && def.deform?.keypoints.length) {
      points.push(...meshBindingPoints(resolvedRig, def, buildSkeletonWarpMesh));
      continue;
    }
    points.push(...transformedBindingBox(binding));
  }
  if (points.length === 0) return null;
  return points.reduce(
    (acc, point) => ({
      minX: Math.min(acc.minX, point.x),
      minY: Math.min(acc.minY, point.y),
      maxX: Math.max(acc.maxX, point.x),
      maxY: Math.max(acc.maxY, point.y)
    }),
    { minX: Number.POSITIVE_INFINITY, minY: Number.POSITIVE_INFINITY, maxX: Number.NEGATIVE_INFINITY, maxY: Number.NEGATIVE_INFINITY }
  );
}

function meshBindingPoints(resolvedRig, def, buildSkeletonWarpMesh) {
  const keypoints = def.deform.keypoints;
  const targetCenters = keypoints.map((keypoint) => resolvedRig.anchorsById[keypoint.anchorId]?.position).filter(Boolean);
  if (targetCenters.length !== keypoints.length) return [];
  const sourceCenters = keypoints.map((keypoint) => ({ x: keypoint.sourceX, y: keypoint.sourceY }));
  const radii = keypoints.map((keypoint) => keypoint.radius);
  const sourceRadii = keypoints.map((keypoint) => keypoint.sourceRadius ?? keypoint.radius);
  const mesh = buildSkeletonWarpMesh(null, def.width, def.height, sourceCenters, targetCenters, sourceRadii, radii, def.deform.segments);
  return mesh ? mesh.targetRows.flat() : targetCenters.flatMap((point, index) => radiusBox(point, radii[index] ?? 0));
}

function transformedBindingBox(binding) {
  const def = binding.definition;
  let local;
  if (def.kind === "image") {
    const x = -(def.pivotX ?? 0.5) * def.width;
    const y = -(def.pivotY ?? 0.5) * def.height;
    local = [
      { x, y },
      { x: x + def.width, y },
      { x: x + def.width, y: y + def.height },
      { x, y: y + def.height }
    ];
  } else if (def.kind === "line" || def.kind === "capsule") {
    local = [
      { x: 0, y: -def.height * 0.5 },
      { x: def.width, y: -def.height * 0.5 },
      { x: def.width, y: def.height * 0.5 },
      { x: 0, y: def.height * 0.5 }
    ];
  } else {
    local = [
      { x: -def.width * 0.5, y: -def.height * 0.5 },
      { x: def.width * 0.5, y: -def.height * 0.5 },
      { x: def.width * 0.5, y: def.height * 0.5 },
      { x: -def.width * 0.5, y: def.height * 0.5 }
    ];
  }
  return local.map((point) => applyMatrix(binding.matrix, point));
}

function radiusBox(point, radius) {
  return [
    { x: point.x - radius, y: point.y - radius },
    { x: point.x + radius, y: point.y + radius }
  ];
}

function applyMatrix(matrix, point) {
  return {
    x: matrix.a * point.x + matrix.c * point.y + matrix.e,
    y: matrix.b * point.x + matrix.d * point.y + matrix.f
  };
}

function renderOptions(values, fittedView, variant) {
  if (variant !== "debug") {
    return {
      showStage: false,
      showBones: false,
      showAnchors: false,
      showBindings: false,
      showImages: true,
      showSkin: true,
      showLabels: false,
      zoom: fittedView.zoom,
      panX: fittedView.panX,
      panY: fittedView.panY,
      background: variant === "clean-black" ? "#070808" : "transparent"
    };
  }

  return {
    showStage: true,
    showBones: true,
    showAnchors: true,
    showBindings: true,
    showImages: true,
    showSkin: true,
    showLabels: true,
    selectedBoneId: values.selectedBone,
    selectedAnchorId: values.selectedAnchor,
    selectedBindingId: values.selectedBinding,
    zoom: fittedView.zoom,
    panX: fittedView.panX,
    panY: fittedView.panY,
    background: "#070808"
  };
}

function drawText(ctx, text, x, y, color, font) {
  ctx.font = font;
  ctx.fillStyle = color;
  ctx.fillText(text, x, y);
}

function drawCheckerboard(ctx, x, y, width, height) {
  const size = 16;
  ctx.fillStyle = "#1b1d1c";
  ctx.fillRect(x, y, width, height);
  for (let yy = y; yy < y + height; yy += size) {
    for (let xx = x; xx < x + width; xx += size) {
      const even = ((xx - x) / size + (yy - y) / size) % 2 === 0;
      ctx.fillStyle = even ? "#2a2d2b" : "#151716";
      ctx.fillRect(xx, yy, Math.min(size, x + width - xx), Math.min(size, y + height - yy));
    }
  }
}
