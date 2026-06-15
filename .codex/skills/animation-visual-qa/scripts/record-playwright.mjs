#!/usr/bin/env node
import fs from "node:fs/promises";
import path from "node:path";

function usage() {
  console.log(`Usage:
  record-playwright.mjs --url <url> --out-dir <dir> [options]

Options:
  --trigger <selector>       Click this selector after loading.
  --wait-for <selector>      Wait for selector before triggering.
  --duration-ms <ms>         Time to record after trigger. Default: 3000.
  --width <px>               Viewport width. Default: 1280.
  --height <px>              Viewport height. Default: 720.
  --headless <0|1>           Run headless. Default: 1.

Requires Playwright in the current project or Node module path.`);
}

const args = new Map();
for (let i = 2; i < process.argv.length; i += 1) {
  const key = process.argv[i];
  if (key === "-h" || key === "--help") {
    usage();
    process.exit(0);
  }
  if (!key.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (!next || next.startsWith("--")) {
    args.set(key.slice(2), "1");
  } else {
    args.set(key.slice(2), next);
    i += 1;
  }
}

const url = args.get("url");
const outDir = args.get("out-dir");
if (!url || !outDir) {
  usage();
  process.exit(2);
}

let chromium;
try {
  ({ chromium } = await import("playwright"));
} catch {
  console.error("Playwright is not available. Install it in the target project or use another recording method.");
  process.exit(1);
}

const width = Number(args.get("width") || 1280);
const height = Number(args.get("height") || 720);
const durationMs = Number(args.get("duration-ms") || 3000);
const headless = args.get("headless") !== "0";

await fs.mkdir(outDir, { recursive: true });

const browser = await chromium.launch({ headless });
const context = await browser.newContext({
  viewport: { width, height },
  deviceScaleFactor: 1,
  recordVideo: { dir: outDir, size: { width, height } }
});

const page = await context.newPage();
await page.goto(url, { waitUntil: "domcontentloaded" });
await page.waitForLoadState("networkidle", { timeout: 5000 }).catch(() => {});

const waitFor = args.get("wait-for");
if (waitFor) await page.waitForSelector(waitFor, { timeout: 15000 });

const trigger = args.get("trigger");
if (trigger) await page.click(trigger);

await page.waitForTimeout(durationMs);

const rawVideo = await page.video()?.path();
await context.close();
await browser.close();

if (!rawVideo) {
  console.error("No video was produced.");
  process.exit(1);
}

const finalPath = path.join(outDir, "recording.webm");
await fs.rename(rawVideo, finalPath).catch(async () => {
  await fs.copyFile(rawVideo, finalPath);
});
console.log(finalPath);
