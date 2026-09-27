#!/usr/bin/env node
import { existsSync, readFileSync, readdirSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { parse as parseYaml } from "yaml";
import { createHash } from "node:crypto";

const clientRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const repoRoot = path.resolve(clientRoot, "..");
const actionsPath = path.join(repoRoot, "resources/scripts/combat_actions/battle-actions.json");
const framesPath = path.join(clientRoot, "src/assets/battle/actors/raster-v1");
const martialArtsPath = path.join(repoRoot, "resources/scripts/martial_arts");
const manifest = JSON.parse(readFileSync(actionsPath, "utf8"));
const atlas = JSON.parse(readFileSync(path.join(clientRoot, 'src/battle/frameAtlas.json'), 'utf8'));
const hash = bytes => createHash('sha256').update(bytes).digest('hex');

if (manifest.schemaVersion !== 4 || !Array.isArray(manifest.actions)) fail("battle-actions.json must use schemaVersion 4 with an actions array");
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
      const png = readFileSync(asset);
      if (png.readUInt32BE(16) !== 256 || png.readUInt32BE(20) !== 192 || png[25] !== 6) fail(`Invalid 256x192 RGBA frame: ${asset}`);
      if (atlas[action.style]?.sources?.[layer]?.[frame.frameId] !== hash(png)) fail(`Stale actor atlas for ${asset}; run npm run pack:battle`);
    }
    durationMs += frame.holdMs;
  }
  if (durationMs !== action.durationMs) fail(`Animation action ${action.id} durationMs=${action.durationMs} does not match frame holds=${durationMs}`);
  const c = action.choreography;
  const impact = action.frames.slice(0, action.impactFrame).reduce((sum, frame) => sum + frame.holdMs, 0);
  if (!c || ![c.launchAtMs, c.hitStopMs, c.recoverAtMs, c.restAtMs, c.reach, c.contactY].every(Number.isFinite)) fail(`${action.id}: missing choreography`);
  if (!(0 <= c.launchAtMs && c.launchAtMs <= impact && c.hitStopMs >= 0 && impact + c.hitStopMs <= c.recoverAtMs && c.recoverAtMs <= c.restAtMs && c.restAtMs <= durationMs)) fail(`${action.id}: unordered choreography markers`);
  if (!['light', 'heavy', 'quiet'].includes(c.weight) || c.reach < 0 || c.reach > 136 || c.contactY < 20 || c.contactY > 176) fail(`${action.id}: invalid contact geometry/weight`);
  if (['approach', 'lunge', 'drive'].includes(action.actorMotion)) {
    const idle = action.style === 'sword' ? 'sword_ready' : 'idle';
    if (action.frames[0].frameId !== idle || action.frames.at(-1).frameId !== idle) fail(`${action.id}: attack must begin and finish in ready stance`);
  }
}

for (const name of ['backdrop', 'thrust', 'slash', 'rising', 'impact', 'parry', 'aura']) {
  if (!existsSync(path.join(clientRoot, 'src/assets/battle/ink-stage-v1', `${name}.webp`))) fail(`Missing generated stage artwork: ${name}`);
}
for (const style of ['fist', 'sword']) for (const layer of ['body', 'hair']) {
  const png = readFileSync(path.join(clientRoot, 'src/assets/battle/ink-stage-v1', `${style}-${layer}-atlas.png`));
  if (hash(png) !== atlas[style].hashes[layer]) fail(`Stale atlas output ${style}/${layer}; run npm run pack:battle`);
  if (png.readUInt32BE(16) !== atlas[style].width || png.readUInt32BE(20) !== atlas[style].height) fail(`Atlas geometry mismatch ${style}/${layer}`);
}
for (const name of ['thrust', 'slash', 'rising', 'impact', 'parry', 'aura']) {
  if (hash(readFileSync(path.join(clientRoot, 'src/assets/battle/ink-stage-v1', `${name}.webp`))) !== atlas.effects?.sources?.[name]) fail(`Stale VFX atlas: ${name}; run npm run pack:battle`);
}
if (hash(readFileSync(path.join(clientRoot, 'src/assets/battle/ink-stage-v1/vfx-atlas.png'))) !== atlas.effects?.hash) fail('Stale VFX atlas output; run npm run pack:battle');

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
