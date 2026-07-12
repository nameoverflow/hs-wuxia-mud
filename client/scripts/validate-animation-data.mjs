#!/usr/bin/env node
import { existsSync, readFileSync, readdirSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { parse as parseYaml } from "yaml";

const clientRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const repoRoot = path.resolve(clientRoot, "..");
const actionsPath = path.join(repoRoot, "resources/scripts/combat_actions/battle-actions.json");
const framesPath = path.join(clientRoot, "src/assets/battle/actors/raster-v1");
const martialArtsPath = path.join(repoRoot, "resources/scripts/martial_arts");
const manifest = JSON.parse(readFileSync(actionsPath, "utf8"));

if (manifest.schemaVersion !== 3 || !Array.isArray(manifest.actions)) fail("battle-actions.json must use schemaVersion 3 with an actions array");
const actionIds = new Set();
const frameIds = new Set();
for (const action of manifest.actions) {
  if (!action.id || actionIds.has(action.id)) fail(`Duplicate or empty rig action id: ${action.id || "<empty>"}`);
  actionIds.add(action.id);
  if (action.frameset !== "raster-v1") fail(`Animation action ${action.id} must use frameset raster-v1`);
  if (action.style !== "fist" && action.style !== "sword") fail(`Animation action ${action.id} has invalid style ${action.style}`);
  if (!Array.isArray(action.frames) || action.frames.length === 0) fail(`Animation action ${action.id} has no frames`);
  if (!Number.isInteger(action.impactFrame) || action.impactFrame < 0 || action.impactFrame >= action.frames.length) {
    fail(`Animation action ${action.id} has invalid impactFrame=${action.impactFrame}`);
  }
  let durationMs = 0;
  for (const frame of action.frames) {
    if (!frame.frameId || !Number.isFinite(frame.holdMs) || frame.holdMs <= 0) fail(`Animation action ${action.id} has an invalid frame`);
    frameIds.add(`${action.style}/${frame.frameId}`);
    for (const layer of ["body", "hair"]) {
      const asset = path.join(framesPath, action.style, layer, `${frame.frameId}.png`);
      if (!existsSync(asset)) fail(`Animation action ${action.id} references missing raster frame ${action.style}/${layer}/${frame.frameId}.png`);
    }
    durationMs += frame.holdMs;
  }
  if (durationMs !== action.durationMs) fail(`Animation action ${action.id} durationMs=${action.durationMs} does not match frame holds=${durationMs}`);
}

let moveCount = 0;
for (const file of readdirSync(martialArtsPath).filter((name) => name.endsWith(".yaml")).sort()) {
  const parsed = parseYaml(readFileSync(path.join(martialArtsPath, file), "utf8"));
  const arts = Array.isArray(parsed) ? parsed : [parsed];
  for (const art of arts.filter(Boolean)) {
    for (const kind of ["attack_moves", "active_skills"]) {
      for (const move of Array.isArray(art[kind]) ? art[kind] : []) {
        moveCount += 1;
        const actionId = move.animation?.action;
        if (typeof actionId !== "string" || !actionId) fail(`${file}: ${art.id}.${move.id} must bind animation.action`);
        if (move.animation?.pool !== undefined) fail(`${file}: ${art.id}.${move.id} still uses animation.pool`);
        if (!actionIds.has(actionId)) fail(`${file}: ${art.id}.${move.id} references missing action ${actionId}`);
      }
    }
  }
}

console.log(`Validated ${manifest.actions.length} battle actions, ${frameIds.size} raster frames, and ${moveCount} fixed move bindings.`);

function fail(message) {
  throw new Error(message);
}
