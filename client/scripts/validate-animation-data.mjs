#!/usr/bin/env node
import { existsSync, readFileSync, readdirSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { parse as parseYaml } from "yaml";
import { createHash } from "node:crypto";
import { readBattleActions } from "./lib/battleActions.mjs";

const clientRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const repoRoot = path.resolve(clientRoot, "..");
const posesPath = path.join(repoRoot, "resources/scripts/combat_presentation/svg-poses.json");
const stagingPath = path.join(repoRoot, "resources/scripts/combat_presentation/staging.json");
const framesPath = path.join(clientRoot, "src/assets/battle/actors/raster-v1");
const martialArtsPath = path.join(repoRoot, "resources/scripts/martial_arts");
const manifest = readBattleActions(repoRoot);
const poseIds = new Set(Object.keys(JSON.parse(readFileSync(posesPath, "utf8")).poses));
const vfxArts = ["impact", "slash", "parry", "aura", "thrust", "rising"];
const stagingPresets = JSON.parse(readFileSync(stagingPath, "utf8")).presets;
const stagingShape = { ...stagingPresets.light, reactions: Object.fromEntries(Object.entries(stagingPresets.light.reactions).map(([k, v]) => [k, { push: 0, tilt: 0, lift: 0, leadMs: 0, onsetMs: 0, ghost: 0, standoff: 0, ...v, pose: "" }])) };
/** staging 覆盖只能写预设里已有的键，数值要是有限数，pose 要在姿势库里。 */
function checkStaging(label, override, shape = stagingShape) {
  if (override === undefined) return;
  if (!override || typeof override !== "object" || Array.isArray(override)) fail(`${label}: staging must be an object`);
  for (const [key, value] of Object.entries(override)) {
    if (!(key in shape)) fail(`${label}: unknown staging key ${key}`);
    if (typeof shape[key] === "object") checkStaging(`${label}.${key}`, value, shape[key]);
    else if (key === "pose") { if (!poseIds.has(value)) fail(`${label}: missing SVG pose ${value}`); }
    else if (!Number.isFinite(value)) fail(`${label}.${key} must be a number`);
  }
}
const travelMotions = ["approach", "lunge", "drive"];
const atlas = JSON.parse(readFileSync(path.join(clientRoot, 'src/battle/frameAtlas.json'), 'utf8'));
const hash = bytes => createHash('sha256').update(bytes).digest('hex');

for (const file of manifest.manifests) if (file.schemaVersion !== 5 || !Array.isArray(file.actions)) fail(`combat_actions/${file.file} must use schemaVersion 5 with an actions array`);
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
  const hits = action.hits ?? [{ atMs: impact, hitStopMs: c.hitStopMs }];
  if (!Array.isArray(hits) || !hits.length) fail(`${action.id}: hits must be a non-empty array`);
  if (hits[0].atMs !== impact) fail(`${action.id}: first hit must land on impactFrame (${impact}ms)`);
  hits.forEach((hit, i) => {
    if (!Number.isFinite(hit.atMs) || !Number.isFinite(hit.hitStopMs) || hit.hitStopMs < 0) fail(`${action.id}: hit ${i} needs atMs/hitStopMs`);
    if (hit.share !== undefined && !(hit.share > 0)) fail(`${action.id}: hit ${i} share must be positive`);
    if (i > 0 && hits[i - 1].atMs + hits[i - 1].hitStopMs > hit.atMs) fail(`${action.id}: hit ${i} starts inside the previous hold`);
  });
  const lastHold = hits.at(-1).atMs + hits.at(-1).hitStopMs;
  checkStaging(`${action.id}.staging`, action.staging);
  hits.forEach((hit, i) => checkStaging(`${action.id}.hits[${i}].staging`, hit.staging));
  if (!['none', 'approach', 'lunge', 'drive', 'focus', 'ranged'].includes(action.actorMotion)) fail(`${action.id}: unknown actorMotion ${action.actorMotion}`);
  if (action.offsetTrack) {
    if (!Array.isArray(action.offsetTrack) || !action.offsetTrack.length) fail(`${action.id}: offsetTrack must be a non-empty array`);
    action.offsetTrack.forEach((key, i) => {
      if (!Number.isFinite(key.atMs) || (i > 0 && key.atMs < action.offsetTrack[i - 1].atMs) || key.atMs > durationMs) fail(`${action.id}: offsetTrack key ${i} out of order`);
      for (const field of ['x', 'y', 'angle']) if (key[field] !== undefined && !Number.isFinite(key[field])) fail(`${action.id}: offsetTrack key ${i}.${field} must be a number`);
      if (key.ease !== undefined && !['cut', 'linear', 'out'].includes(key.ease)) fail(`${action.id}: offsetTrack key ${i} has invalid ease`);
    });
  }
  if (!(0 <= c.launchAtMs && c.launchAtMs <= impact && c.hitStopMs >= 0 && lastHold <= c.recoverAtMs && c.recoverAtMs <= c.restAtMs && c.restAtMs <= durationMs)) fail(`${action.id}: unordered choreography markers`);
  if (!['light', 'heavy', 'quiet'].includes(c.weight) || c.reach < 0 || c.reach > 160 || c.contactY < 20 || c.contactY > 176) fail(`${action.id}: invalid contact geometry/weight`);
  for (const frame of action.frames) if (!poseIds.has(frame.frameId)) fail(`${action.id}: frame ${frame.frameId} has no SVG pose`);
  const kp = action.keyPoses;
  const track = action.poseTrack;
  for (const id of [kp?.prepare, kp?.contact, kp?.finish, action.approach?.pose, ...(track ?? []).map((key) => key.pose)].filter(Boolean)) if (!poseIds.has(id)) fail(`${action.id}: missing SVG pose ${id}`);
  if (track) {
    if (!Array.isArray(track) || !track.length || track[0].atMs !== 0) fail(`${action.id}: poseTrack must start at 0ms`);
    track.forEach((key, i) => {
      if (!Number.isFinite(key.atMs) || (i > 0 && key.atMs < track[i - 1].atMs) || key.atMs > durationMs) fail(`${action.id}: poseTrack key ${i} out of order`);
      if (key.pin !== undefined && !['hand', 'foot', 'blade'].includes(key.pin)) fail(`${action.id}: poseTrack key ${i} has invalid pin`);
    });
    if (travelMotions.includes(action.actorMotion) && !track.some((key) => key.pin)) fail(`${action.id}: poseTrack needs at least one pinned contact key`);
  }
  for (const [i, vfx] of (action.vfx || []).entries()) {
    const label = `${action.id}: vfx[${i}]`;
    if (!['trail', 'impact', 'parry', 'aura', 'heal', 'sprite', 'custom'].includes(vfx.kind)) fail(`${label} has unknown kind ${vfx.kind}`);
    if (!vfxArts.includes(vfx.art)) fail(`${label} needs art in ${vfxArts.join('/')}`);
    if (vfx.kind !== 'sprite' && vfx.kind !== 'custom') continue;
    const anchors = ['contact', 'actor', 'target', 'center', 'actor.hand', 'actor.foot', 'actor.blade'];
    for (const key of ['from', 'to']) if (vfx[key] !== undefined && !anchors.includes(vfx[key])) fail(`${label}.${key} must be one of ${anchors.join('/')}`);
    for (const key of ['atMs', 'offsetMs', 'size', 'rotate', 'spin', 'opacity', 'fadeInMs', 'fadeOutMs']) if (vfx[key] !== undefined && !Number.isFinite(vfx[key])) fail(`${label}.${key} must be a number`);
    if (vfx.durationMs !== undefined && !(vfx.durationMs > 0)) fail(`${label}.durationMs must be positive`);
    if (vfx.hit !== undefined && !(Number.isInteger(vfx.hit) && vfx.hit >= 0 && vfx.hit < (action.hits?.length ?? 1))) fail(`${label}.hit is out of range`);
    if (vfx.scale !== undefined && !(Array.isArray(vfx.scale) && vfx.scale.length === 2 && vfx.scale.every(Number.isFinite))) fail(`${label}.scale must be [from, to]`);
    if (vfx.results !== undefined && !(Array.isArray(vfx.results) && vfx.results.every((r) => ['hit', 'dodge', 'parry', 'effect'].includes(r)))) fail(`${label}.results has an unknown result`);
    if (vfx.kind === 'custom' && (typeof vfx.effect !== 'string' || !vfx.effect)) fail(`${label} needs an effect name`);
  }
  if (action.actorMotion === 'ranged' && !track && !(kp?.prepare && kp.contact && kp.finish)) fail(`${action.id}: ranged action needs a poseTrack or keyPoses`);
  if (travelMotions.includes(action.actorMotion) && track && track.filter((key) => key.pin).length !== hits.length) fail(`${action.id}: each hit needs exactly one pinned poseTrack key`);
  if (travelMotions.includes(action.actorMotion)) {
    // 剑光在最后一段定格后还要收笔，收不完就会残留在下一招开头。
    const fadeMs = action.staging?.trail?.fadeMs ?? stagingPresets[c.weight]?.trail?.fadeMs ?? stagingPresets.light.trail.fadeMs;
    if (lastHold + fadeMs > durationMs) fail(`${action.id}: last hit ends ${durationMs - lastHold}ms before the clip ends, but the trail needs ${fadeMs}ms to fade`);
    const idle = action.style === 'sword' ? 'sword_ready' : 'idle';
    if (action.frames[0].frameId !== idle || action.frames.at(-1).frameId !== idle) fail(`${action.id}: attack must begin and finish in ready stance`);
    if (!track) {
      if (!kp?.prepare || !kp.contact || !kp.finish || !['hand', 'foot', 'blade'].includes(kp.reachWith)) fail(`${action.id}: attack needs keyPoses prepare/contact/finish/reachWith or a poseTrack`);
      if (kp.contact !== action.frames[action.impactFrame].frameId) fail(`${action.id}: keyPoses.contact must match the impact frame`);
    }
    if (!action.approach || !(action.approach.durationMs > 0) || !Number.isFinite(action.approach.lift)) fail(`${action.id}: attack needs approach pose/durationMs/lift`);
  }
  if (action.actorMotion === 'focus' && !kp?.contact) fail(`${action.id}: focus action needs keyPoses.contact`);
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
        const params = move.animation?.params;
        if (params !== undefined) {
          const label = `${file}: ${art.id}.${move.id}.animation.params`;
          if (!params || typeof params !== "object" || Array.isArray(params)) fail(`${label} must be a mapping`);
          for (const key of Object.keys(params)) if (!["label", "staging", "vfxArt"].includes(key)) fail(`${label} has unknown key ${key}`);
          if (params.label !== undefined && (typeof params.label !== "string" || !params.label.trim())) fail(`${label}.label must be text`);
          checkStaging(`${label}.staging`, params.staging);
          for (const [kind, artName] of Object.entries(params.vfxArt || {})) {
            if (!["trail", "impact", "parry", "aura", "heal", "sprite", "custom"].includes(kind)) fail(`${label}.vfxArt has unknown kind ${kind}`);
            if (!vfxArts.includes(artName)) fail(`${label}.vfxArt.${kind} must be one of ${vfxArts.join("/")}`);
          }
        }
      }
    }
  }
}

console.log(`Validated ${manifest.actions.length} battle actions, ${frameIds.size} raster frames, and ${moveCount} fixed move bindings.`);

function fail(message) {
  throw new Error(message);
}
