import { createCanvas, loadImage } from '@napi-rs/canvas';
import { readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';

const client = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const data = JSON.parse(readFileSync(path.join(client, '../resources/scripts/combat_actions/battle-actions.json'), 'utf8'));
const atlas = {};
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
for (const style of ['fist', 'sword']) {
  const names = [...new Set(data.actions.filter(a => a.style === style).flatMap(a => a.frames.map(f => f.frameId)))].sort();
  const width = 1024, height = Math.ceil(names.length / 4) * 192;
  atlas[style] = { width, height, frames: Object.fromEntries(names.map((name, i) => [name, { x: i % 4 * 256, y: Math.floor(i / 4) * 192 }])), sources: {}, hashes: {} };
  for (const layer of ['body', 'hair']) {
    const canvas = createCanvas(width, height), ctx = canvas.getContext('2d');
    atlas[style].sources[layer] = {};
    for (const name of names) {
      const file = path.join(client, 'src/assets/battle/actors/raster-v1', style, layer, `${name}.png`);
      const image = await loadImage(file);
      atlas[style].sources[layer][name] = hash(readFileSync(file));
      const pos = atlas[style].frames[name];
      ctx.drawImage(image, pos.x, pos.y);
    }
    // Mechanical palette normalization only. Every original alpha pixel is retained.
    ctx.globalCompositeOperation = 'source-in';
    ctx.fillStyle = '#ffffff';
    ctx.fillRect(0, 0, width, height);
    const png = canvas.toBuffer('image/png');
    atlas[style].hashes[layer] = hash(png);
    writeFileSync(path.join(client, 'src/assets/battle/ink-stage-v1', `${style}-${layer}-atlas.png`), png);
  }
}
const effects = ['thrust', 'slash', 'rising', 'impact', 'parry', 'aura'];
const effectCanvas = createCanvas(768, 512), effectContext = effectCanvas.getContext('2d');
atlas.effects = { width: 768, height: 512, sources: {}, hash: '' };
for (let i = 0; i < effects.length; i++) {
  const file = path.join(client, 'src/assets/battle/ink-stage-v1', `${effects[i]}.webp`);
  const image = await loadImage(file);
  atlas.effects.sources[effects[i]] = hash(readFileSync(file));
  effectContext.drawImage(image, i % 3 * 256, Math.floor(i / 3) * 256);
}
const effectPng = effectCanvas.toBuffer('image/png');
atlas.effects.hash = hash(effectPng);
writeFileSync(path.join(client, 'src/assets/battle/ink-stage-v1/vfx-atlas.png'), effectPng);
writeFileSync(path.join(client, 'src/battle/frameAtlas.json'), JSON.stringify(atlas, null, 2) + '\n');
console.log('Packed original generated alpha into four actor atlases and one VFX atlas.');
