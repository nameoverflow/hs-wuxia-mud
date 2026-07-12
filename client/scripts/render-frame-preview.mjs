#!/usr/bin/env node
import { createCanvas, loadImage } from "@napi-rs/canvas";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const clientRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const repoRoot = path.resolve(clientRoot, "..");
const manifest = JSON.parse(readFileSync(path.join(repoRoot, "resources/scripts/combat_actions/battle-actions.json"), "utf8"));
const options = parseArgs(process.argv.slice(2));
const action = manifest.actions.find((candidate) => candidate.id === options.action);
if (!action) throw new Error(`Unknown battle action ${options.action}`);

const frameWidth = 256;
const frameHeight = 192;
const labelHeight = 24;
const sampleCount = Math.max(1, Math.ceil((action.durationMs / 1000) * options.fps));
const rows = Math.ceil(sampleCount / options.cols);
const sheet = createCanvas(options.cols * frameWidth, rows * (frameHeight + labelHeight));
const ctx = sheet.getContext("2d");
ctx.fillStyle = "#181b1c";
ctx.fillRect(0, 0, sheet.width, sheet.height);
ctx.font = "13px ui-monospace, SFMono-Regular, Menlo, monospace";
ctx.textBaseline = "middle";

const cache = new Map();
for (let index = 0; index < sampleCount; index += 1) {
  const elapsedMs = Math.min(action.durationMs - 1, Math.round((index / options.fps) * 1000));
  const frame = frameAt(action.frames, elapsedMs);
  const col = index % options.cols;
  const row = Math.floor(index / options.cols);
  const x = col * frameWidth;
  const y = row * (frameHeight + labelHeight);
  if (options.profile === "female") ctx.drawImage(await imageFor(action.style, "hair", frame.frameId), x, y, frameWidth, frameHeight);
  ctx.drawImage(await imageFor(action.style, "body", frame.frameId), x, y, frameWidth, frameHeight);
  ctx.fillStyle = index === 0 || frame.frameId !== frameAt(action.frames, Math.max(0, elapsedMs - 1000 / options.fps)).frameId ? "#e2bd55" : "#9ba19d";
  ctx.fillText(`${elapsedMs}ms · ${frame.frameId}`, x + 7, y + frameHeight + labelHeight * 0.5);
}

mkdirSync(path.dirname(options.out), { recursive: true });
writeFileSync(options.out, sheet.toBuffer("image/png"));
console.log(JSON.stringify({ out: path.relative(repoRoot, options.out), action: action.id, profile: options.profile, samples: sampleCount, fps: options.fps }, null, 2));

function frameAt(frames, elapsedMs) {
  let cursor = Math.max(0, elapsedMs);
  for (const frame of frames) {
    if (cursor < frame.holdMs) return frame;
    cursor -= frame.holdMs;
  }
  return frames[frames.length - 1];
}

async function imageFor(style, layer, frameId) {
  const key = `${style}/${layer}/${frameId}`;
  if (!cache.has(key)) cache.set(key, loadImage(path.join(clientRoot, "src/assets/battle/actors/raster-v1", style, layer, `${frameId}.png`)));
  return cache.get(key);
}

function parseArgs(args) {
  const values = {
    action: "rig.fist.punch_a",
    profile: "female",
    fps: 16,
    cols: 8,
    out: path.join(repoRoot, "harness/tmp/raster-frame-preview.png")
  };
  for (let index = 0; index < args.length; index += 2) {
    const flag = args[index];
    const value = args[index + 1];
    if (!flag?.startsWith("--") || value === undefined) throw new Error(`Invalid argument ${flag || "<empty>"}`);
    if (flag === "--action") values.action = value;
    else if (flag === "--profile") values.profile = value;
    else if (flag === "--fps") values.fps = Number(value);
    else if (flag === "--cols") values.cols = Number(value);
    else if (flag === "--out") values.out = path.resolve(repoRoot, value);
    else throw new Error(`Unknown option ${flag}`);
  }
  if (values.profile !== "male" && values.profile !== "female") throw new Error("profile must be male or female");
  if (!Number.isFinite(values.fps) || values.fps <= 0 || !Number.isInteger(values.cols) || values.cols <= 0) throw new Error("fps and cols must be positive");
  return values;
}
