import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: './tests',
  outputDir: '../harness/tmp/silhouette-stage-v2/test-results',
  fullyParallel: false,
  workers: 1,
  use: { baseURL: 'http://127.0.0.1:8080', viewport: { width: 1100, height: 950 }, channel: process.env.PLAYWRIGHT_CHANNEL || undefined, trace: 'retain-on-failure' },
  webServer: { command: 'npm run dev -- --port 8080', url: 'http://127.0.0.1:8080/battle-lab.html', reuseExistingServer: !process.env.CI }
});
