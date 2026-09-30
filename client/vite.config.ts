import { svelte } from "@sveltejs/vite-plugin-svelte";
import { fileURLToPath, URL } from "node:url";
import { defineConfig } from "vite";

export default defineConfig({
  plugins: [svelte()],
  build: {
    rollupOptions: {
      input: {
        main: fileURLToPath(new URL("./index.html", import.meta.url)),
        battleLab: fileURLToPath(new URL("./battle-lab.html", import.meta.url)),
        poseSheet: fileURLToPath(new URL("./pose-sheet.html", import.meta.url))
      }
    }
  },
  server: {
    host: "127.0.0.1",
    port: 8080,
    strictPort: false,
    fs: {
      allow: [fileURLToPath(new URL("..", import.meta.url))]
    }
  },
  preview: {
    host: "127.0.0.1",
    port: 8080,
    strictPort: false
  }
});
