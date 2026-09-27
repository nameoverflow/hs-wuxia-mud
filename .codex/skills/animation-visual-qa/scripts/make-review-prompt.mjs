#!/usr/bin/env node
import fs from "node:fs/promises";
import path from "node:path";

function usage() {
  console.log(`Usage:
  make-review-prompt.mjs --expectation <text> --storyboards <dir> --out <review.md> [--recording <video>]
`);
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

const expectation = args.get("expectation");
const storyboardsDir = args.get("storyboards");
const out = args.get("out");

if (!expectation || !storyboardsDir || !out) {
  usage();
  process.exit(2);
}

const files = (await fs.readdir(storyboardsDir))
  .filter((name) => /^storyboard-\d+\.png$/i.test(name))
  .sort()
  .map((name) => path.resolve(storyboardsDir, name));

if (!files.length) {
  console.error(`No storyboard-*.png files found in ${storyboardsDir}`);
  process.exit(1);
}

const recording = args.get("recording") ? path.resolve(args.get("recording")) : "";
const body = `# Animation Visual QA Review Prompt

Expectation:
${expectation}

Artifacts:
${recording ? `- recording: ${recording}\n` : ""}${files.map((file) => `- storyboard: ${file}`).join("\n")}

Instructions for Codex:

1. Inspect every storyboard image with view_image in listed order.
2. Treat each storyboard as a time sequence, reading left-to-right and top-to-bottom.
3. Judge whether the animation matches the expectation subjectively.
4. Focus on action semantics, rhythm, impact, continuity, composition, and wuxia silhouette style.
5. Return:
   - Verdict: Fail, Borderline, or Pass
   - Actual findings with segment/frame-region evidence; do not pad the list
   - Recommended animation changes
6. Use a numeric score only when the comparison needs it, with an explicit rubric.
`;

await fs.mkdir(path.dirname(path.resolve(out)), { recursive: true });
await fs.writeFile(out, body, "utf8");
console.log(path.resolve(out));
