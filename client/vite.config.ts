import { svelte } from "@sveltejs/vite-plugin-svelte";
import { readFile, writeFile } from "node:fs/promises";
import type { IncomingMessage } from "node:http";
import { fileURLToPath, URL } from "node:url";
import { defineConfig, type Plugin } from "vite";

const segmentedPosePath = fileURLToPath(new URL("./src/battle/skeletal/data/segmented-v12-poses.json", import.meta.url));

export default defineConfig({
  plugins: [svelte(), rigPoseSavePlugin()],
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

function rigPoseSavePlugin(): Plugin {
  return {
    name: "wuxia-rig-pose-save",
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
