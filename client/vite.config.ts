import { svelte } from "@sveltejs/vite-plugin-svelte";
import { defineConfig } from "vite";
import { fileURLToPath, URL } from "node:url";

export default defineConfig({
  plugins: [svelte()],
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
