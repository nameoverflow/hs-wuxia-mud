import { svelte } from "@sveltejs/vite-plugin-svelte";
import { defineConfig } from "vite";

export default defineConfig({
  plugins: [svelte()],
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
