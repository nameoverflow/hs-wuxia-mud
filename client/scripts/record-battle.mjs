import { chromium } from '@playwright/test';
import { mkdir, writeFile } from 'node:fs/promises';
import path from 'node:path';

const out = path.resolve(process.env.BATTLE_QA_OUT || '../harness/tmp/silhouette-stage-v2/recording');
await mkdir(out, { recursive: true });
const browser = await chromium.launch({ channel: process.env.PLAYWRIGHT_CHANNEL || undefined });
const context = await browser.newContext({ viewport: { width: 640, height: 260 }, recordVideo: { dir: out, size: { width: 640, height: 260 } } });
const page = await context.newPage();
const errors = [];
page.on('pageerror', error => errors.push(error.message));
try {
  await page.goto(`${process.env.BATTLE_QA_URL || 'http://127.0.0.1:8080'}/battle-lab.html`);
  await page.waitForFunction(() => window.__battleLab?.ready());
  // Isolate the actual production stage, preserving its desktop height and renderer.
  await page.addStyleTag({ content: 'body{overflow:hidden}.battle-stage{position:fixed!important;inset:0!important;width:100vw!important;height:100vh!important;margin:0!important;z-index:999;border:0}' });
  await page.waitForTimeout(300);
  const startedAt = await page.evaluate(() => { window.__battleLab.demo(); return performance.now(); });
  await page.waitForFunction(() => !window.__battleLab.state().game.battle.active, null, { timeout: 20000 });
  const endedAt = await page.evaluate(() => performance.now());
  await writeFile(path.join(out, 'capture.json'), JSON.stringify({ startedAt, endedAt, viewport: { width: 640, height: 260 }, errors }, null, 2));
} finally {
  const video = page.video();
  await context.close();
  await video.saveAs(path.join(out, 'recording.webm'));
  await browser.close();
}
console.log(path.join(out, 'recording.webm'));
if (errors.length) { console.error(errors); process.exitCode = 1; }
