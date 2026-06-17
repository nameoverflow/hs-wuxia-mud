import { svelte } from "@sveltejs/vite-plugin-svelte";
import { readdir, readFile, writeFile } from "node:fs/promises";
import type { IncomingMessage } from "node:http";
import path from "node:path";
import { fileURLToPath, URL } from "node:url";
import { defineConfig, type Plugin } from "vite";

const segmentedPosePath = fileURLToPath(new URL("./src/battle/skeletal/data/segmented-v12-poses.json", import.meta.url));
const rigActionPath = fileURLToPath(new URL("./src/battle/skeletal/data/rig-actions.json", import.meta.url));
const martialArtsPath = fileURLToPath(new URL("../resources/scripts/martial_arts", import.meta.url));

export default defineConfig({
  plugins: [svelte(), rigProjectSavePlugin()],
  build: {
    rollupOptions: {
      input: {
        main: fileURLToPath(new URL("./index.html", import.meta.url)),
        animationRig: fileURLToPath(new URL("./animation-rig.html", import.meta.url))
      }
    }
  },
  server: {
    host: "127.0.0.1",
    port: 8080,
    strictPort: false
  },
  preview: {
    host: "127.0.0.1",
    port: 8080,
    strictPort: false
  }
});

function rigProjectSavePlugin(): Plugin {
  return {
    name: "wuxia-rig-project-save",
    configureServer(server) {
      server.middlewares.use("/__rig/segmented-v12-poses", async (req, res) => {
        try {
          if (req.method === "GET") {
            res.setHeader("Content-Type", "application/json");
            res.end(await readFile(segmentedPosePath, "utf8"));
            return;
          }

          if (req.method !== "POST") {
            res.statusCode = 405;
            res.end("Method Not Allowed");
            return;
          }

          const body = await readRequestBody(req);
          const parsed = JSON.parse(body) as { poses?: unknown };
          if (!isPoseLibrary(parsed.poses)) {
            res.statusCode = 400;
            res.end("Invalid pose library");
            return;
          }

          await writeFile(segmentedPosePath, `${JSON.stringify(parsed.poses, null, 2)}\n`);
          res.setHeader("Content-Type", "application/json");
          res.end(JSON.stringify({ ok: true, path: segmentedPosePath }));
        } catch (error) {
          res.statusCode = 500;
          res.end(error instanceof Error ? error.message : "Save failed");
        }
      });

      server.middlewares.use("/__rig/actions", async (req, res) => {
        try {
          if (req.method === "GET") {
            res.setHeader("Content-Type", "application/json");
            res.end(await readFile(rigActionPath, "utf8"));
            return;
          }

          if (req.method !== "POST") {
            res.statusCode = 405;
            res.end("Method Not Allowed");
            return;
          }

          const body = await readRequestBody(req);
          const parsed = JSON.parse(body) as unknown;
          if (!isRigActionManifest(parsed)) {
            res.statusCode = 400;
            res.end("Invalid rig action manifest");
            return;
          }

          await writeFile(rigActionPath, `${JSON.stringify(parsed, null, 2)}\n`);
          res.setHeader("Content-Type", "application/json");
          res.end(JSON.stringify({ ok: true, path: rigActionPath }));
        } catch (error) {
          res.statusCode = 500;
          res.end(error instanceof Error ? error.message : "Save failed");
        }
      });

      server.middlewares.use("/__rig/martial-arts", async (req, res) => {
        try {
          if (req.method === "GET") {
            const files = (await readdir(martialArtsPath)).filter((file) => file.endsWith(".yaml")).sort();
            const payload = await Promise.all(
              files.map(async (file) => ({
                file,
                text: await readFile(path.join(martialArtsPath, file), "utf8")
              }))
            );
            res.setHeader("Content-Type", "application/json");
            res.end(JSON.stringify({ files: payload }));
            return;
          }

          if (req.method !== "POST") {
            res.statusCode = 405;
            res.end("Method Not Allowed");
            return;
          }

          const body = await readRequestBody(req);
          const parsed = JSON.parse(body) as { file?: unknown; text?: unknown };
          if (typeof parsed.file !== "string" || typeof parsed.text !== "string" || !parsed.file.endsWith(".yaml") || path.basename(parsed.file) !== parsed.file) {
            res.statusCode = 400;
            res.end("Invalid martial art file save");
            return;
          }

          await writeFile(path.join(martialArtsPath, parsed.file), parsed.text);
          res.setHeader("Content-Type", "application/json");
          res.end(JSON.stringify({ ok: true, file: parsed.file }));
        } catch (error) {
          res.statusCode = 500;
          res.end(error instanceof Error ? error.message : "Save failed");
        }
      });
    }
  };
}

function readRequestBody(req: IncomingMessage) {
  return new Promise<string>((resolve, reject) => {
    let body = "";
    req.setEncoding("utf8");
    req.on("data", (chunk) => {
      body += chunk;
      if (body.length > 2_000_000) reject(new Error("Request body too large"));
    });
    req.on("end", () => resolve(body));
    req.on("error", reject);
  });
}

function isPoseLibrary(value: unknown): value is Record<string, unknown> {
  return (
    !!value &&
    typeof value === "object" &&
    !Array.isArray(value) &&
    Object.entries(value).every(([id, pose]) => {
      if (!pose || typeof pose !== "object" || Array.isArray(pose)) return false;
      const candidate = pose as { id?: unknown; name?: unknown; bones?: unknown };
      return id.length > 0 && typeof candidate.id === "string" && typeof candidate.name === "string" && isPlainObject(candidate.bones);
    })
  );
}

function isPlainObject(value: unknown): value is Record<string, unknown> {
  return !!value && typeof value === "object" && !Array.isArray(value);
}

function isRigActionManifest(value: unknown): value is Record<string, unknown> {
  if (!isPlainObject(value)) return false;
  const candidate = value as { schemaVersion?: unknown; actions?: unknown };
  return (
    typeof candidate.schemaVersion === "number" &&
    Array.isArray(candidate.actions) &&
    candidate.actions.every((action) => {
      if (!isPlainObject(action)) return false;
      const item = action as { id?: unknown; label?: unknown; rig?: unknown; style?: unknown; poseId?: unknown; durationMs?: unknown; sequence?: unknown; tags?: unknown };
      return (
        typeof item.id === "string" &&
        typeof item.label === "string" &&
        item.rig === "segmented-v12" &&
        (item.style === "sword" || item.style === "fist") &&
        typeof item.poseId === "string" &&
        typeof item.durationMs === "number" &&
        Array.isArray(item.sequence) &&
        item.sequence.every((poseId) => typeof poseId === "string") &&
        Array.isArray(item.tags) &&
        item.tags.every((tag) => typeof tag === "string")
      );
    })
  );
}
