import { test, expect } from '@playwright/test';

test.beforeEach(async ({ page }) => {
  await page.goto('/battle-lab.html');
  await page.waitForFunction(() => (window as any).__battleLab?.ready());
});

async function frozenAction(page: any, action = 'rig.fist.punch_a', outcome = 'hit') {
  await page.evaluate(({ action, outcome }: any) => { const lab = (window as any).__battleLab; lab.seed(); lab.speed(0.1); lab.submit(action, outcome); }, { action, outcome });
  await page.waitForFunction(() => (window as any).__battleLab.state().game.battle.animation.activeTimeline);
  await page.evaluate(() => (window as any).__battleLab.pause());
  return page.evaluate(() => (window as any).__battleLab.state().game.battle.animation.activeTimeline);
}

test('health and defender stay unchanged before contact, then commit exactly once', async ({ page }) => {
  const timeline = await frozenAction(page);
  await page.evaluate((t) => (window as any).__battleLab.seek(t), timeline.impactAtMs - 1);
  await expect(page.locator('[data-side="enemy"]')).toHaveAttribute('data-frame', 'sword_ready');
  expect(await page.evaluate(() => (window as any).__battleLab.state().game.battle.presentation.enemyHp)).toBe(180);
  expect(await page.evaluate(() => (window as any).__battleLab.state().game.battle.enemy.combatantSnapshotHp)).toBe(162);
  await page.evaluate((t) => { const lab = (window as any).__battleLab; lab.seek(t); lab.pause(false); }, timeline.impactAtMs);
  await page.waitForFunction(() => (window as any).__battleLab.state().game.battle.presentation.enemyHp === 162);
  await page.evaluate(() => (window as any).__battleLab.pause());
  await expect(page.locator('[data-side="enemy"]')).toHaveAttribute('data-frame', 'sword_hurt');
  await page.evaluate((duration) => { const lab = (window as any).__battleLab; lab.seek(duration - 1); lab.speed(1); lab.pause(false); }, timeline.durationMs);
  await page.waitForFunction(() => !(window as any).__battleLab.state().game.battle.animation.activeTimeline);
  expect(await page.evaluate(() => (window as any).__battleLab.state().game.battle.presentation.enemyHp)).toBe(162);
});

test('two queued identical punches both contain preparation and strike', async ({ page }) => {
  await page.evaluate(() => {
    const lab = (window as any).__battleLab; lab.seed(); lab.speed(0.5);
    (window as any).__samples = [];
    const collect = () => {
      const s = lab.state();
      (window as any).__samples.push({ id: s.game.battle.animation.activeTimeline?.id, frame: document.querySelector('[data-side="player"]')?.getAttribute('data-frame') });
      if (s.game.battle.animation.queueDepth || (window as any).__samples.length < 4) requestAnimationFrame(collect);
    };
    lab.submit('rig.fist.punch_a'); lab.submit('rig.fist.punch_a'); requestAnimationFrame(collect);
  });
  await page.waitForFunction(() => (window as any).__samples.length > 10 && !(window as any).__battleLab.state().game.battle.animation.queueDepth);
  const samples = await page.evaluate(() => (window as any).__samples);
  const ids = [...new Set(samples.map((s: any) => s.id).filter(Boolean))];
  expect(ids).toHaveLength(2);
  for (const id of ids) {
    const frames = samples.filter((s: any) => s.id === id).map((s: any) => s.frame);
    expect(frames).toContain('punch_windup'); expect(frames).toContain('punch_strike');
  }
});

test('dodge anticipates contact and never produces a damage spark', async ({ page }) => {
  const timeline = await frozenAction(page, 'rig.fist.kick_a', 'dodge');
  await page.evaluate((t) => (window as any).__battleLab.seek(t), timeline.impactAtMs - 30);
  await expect(page.locator('[data-side="enemy"]')).toHaveAttribute('data-frame', 'sword_dodge');
  const frame = await page.evaluate((t) => (window as any).__battleLab.inspectAt(t), timeline.impactAtMs);
  expect(frame.enemy.x).toBeGreaterThan(20); expect(frame.burst).toBe(0);
  expect(await page.evaluate(() => (window as any).__battleLab.state().game.battle.presentation.enemyHp)).toBe(180);
});

test('self healing leaves the opponent alone and updates health at the effect marker', async ({ page }) => {
  await page.evaluate(() => { const lab = (window as any).__battleLab; lab.seed(); lab.submit('rig.sword.cut_a', 'hit', 'enemy', 30); });
  await page.waitForFunction(() => !(window as any).__battleLab.state().game.battle.animation.queueDepth);
  await page.evaluate(() => { const lab = (window as any).__battleLab; lab.speed(0.1); lab.submit('rig.fist.healing_palm', 'effect'); });
  await page.waitForFunction(() => (window as any).__battleLab.state().game.battle.animation.activeTimeline);
  await page.evaluate(() => (window as any).__battleLab.pause());
  expect(await page.evaluate(() => (window as any).__battleLab.state().game.battle.presentation.playerHp)).toBe(150);
  await page.evaluate(() => { const lab = (window as any).__battleLab; lab.seek(lab.state().game.battle.animation.activeTimeline.impactAtMs); lab.pause(false); });
  await page.waitForFunction(() => (window as any).__battleLab.state().game.battle.presentation.playerHp === 172);
  await expect(page.locator('[data-side="enemy"]')).toHaveAttribute('data-frame', 'sword_ready');
});

test('full burst reaches settlement after the final hit and drains the queue', async ({ page }) => {
  const errors: string[] = []; page.on('pageerror', e => errors.push(e.message));
  await page.evaluate(() => (window as any).__battleLab.demo());
  await page.waitForFunction(() => (window as any).__battleLab.state().game.battle.animation.activeTimeline?.kind === 'settlement', { timeout: 15000 });
  expect(await page.evaluate(() => (window as any).__battleLab.state().game.battle.presentation.enemyHp)).toBe(0);
  await page.waitForFunction(() => !(window as any).__battleLab.state().game.battle.active);
  expect(await page.evaluate(() => (window as any).__battleLab.state().game.battle.animation.queueDepth)).toBe(0);
  expect(errors).toEqual([]);
});

test('main game mounts the same stage and hidden tabs do not accumulate stale replays', async ({ page }) => {
  await page.goto('/');
  await page.evaluate(async () => {
    const fixtures = await import('/src/battle/battleFixtures.ts');
    fixtures.seedBattle(); fixtures.submitBattleAction('rig.fist.punch_a');
  });
  await expect(page.locator('.silhouette-stage')).toBeVisible();
  await page.evaluate(async () => {
    Object.defineProperty(document, 'hidden', { configurable: true, get: () => true });
    document.dispatchEvent(new Event('visibilitychange'));
    const fixtures = await import('/src/battle/battleFixtures.ts');
    fixtures.submitBattleAction('rig.fist.punch_a');
  });
  await expect(page.locator('.silhouette-stage')).toHaveAttribute('data-event-id', 'idle');
  await expect(page.locator('.combatant.enemy .hp em')).toHaveText('144/180');
  expect(await page.locator('.log-line').count()).toBeGreaterThanOrEqual(2);
  await page.evaluate(() => { delete (document as any).hidden; document.dispatchEvent(new Event('visibilitychange')); });
  await page.screenshot({ path: '../harness/tmp/silhouette-stage-v2/main-game.png', fullPage: true });
});

test('sound is opt-in and can be enabled by a user gesture', async ({ page }) => {
  const sound = page.getByRole('button', { name: '音效：关' });
  await expect(sound).toHaveAttribute('aria-pressed', 'false');
  await sound.click();
  await expect(page.getByRole('button', { name: '音效：开' })).toHaveAttribute('aria-pressed', 'true');
  await page.evaluate(() => (window as any).__battleLab.submit('rig.sword.thrust_a', 'parry', 'enemy'));
  await page.waitForFunction(() => !(window as any).__battleLab.state().game.battle.animation.queueDepth);
  await page.getByRole('button', { name: '音效：开' }).click();
  await expect(page.getByRole('button', { name: '音效：关' })).toHaveAttribute('aria-pressed', 'false');
});

test('mobile reduced motion remains readable without camera or dash movement', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.emulateMedia({ reducedMotion: 'reduce' });
  const timeline = await frozenAction(page, 'rig.fist.heavy_a');
  await page.evaluate((t) => (window as any).__battleLab.seek(t), timeline.impactAtMs + 60);
  await expect(page.locator('[data-side="player"]')).toHaveCSS('transform', 'matrix(1, 0, 0, 1, 0, 0)');
  await expect(page.locator('[data-side="enemy"]')).toHaveAttribute('data-frame', 'sword_hurt');
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
  await page.screenshot({ path: '../harness/tmp/silhouette-stage-v2/mobile-reduced.png', fullPage: true });
});


test('SVG renderer articulates limbs on seek without loading actor atlases', async ({ page }) => {
  const timeline = await frozenAction(page);
  await expect(page.locator('.silhouette-stage')).toHaveAttribute('data-renderer', 'svg');
  const actor = page.locator('[data-side="player"] svg');
  await expect(actor).toHaveCount(1);
  await page.evaluate(() => (window as any).__battleLab.seek(0));
  const idle = await actor.innerHTML();
  await page.evaluate((t) => (window as any).__battleLab.seek(t), timeline.impactAtMs);
  const contact = await actor.innerHTML();
  expect(contact).not.toBe(idle);
  await page.evaluate((t) => (window as any).__battleLab.seek(t), timeline.impactAtMs + timeline.choreography.hitStopMs - 1);
  expect(await actor.innerHTML()).toBe(contact);
  expect(await page.evaluate(() => performance.getEntriesByType('resource').some(r => /(?:body|hair|vfx)-atlas/.test(r.name)))).toBe(false);
});



test('dash arrives before the restored broad attack', async ({ page }) => {
  const timeline = await frozenAction(page, 'rig.sword.chop_a');
  await page.evaluate((t) => (window as any).__battleLab.seek(t), timeline.actor.actionDelayMs * .7);
  await expect(page.locator('.silhouette-stage')).toHaveAttribute('data-phase', 'approach');
  await expect(page.locator('[data-side="player"]')).toHaveAttribute('data-frame', 'approach-raised-step');
  await page.evaluate((t) => (window as any).__battleLab.seek(t), timeline.actor.actionDelayMs);
  await expect(page.locator('.silhouette-stage')).toHaveAttribute('data-phase', 'prepare');
  expect(await page.evaluate(() => (window as any).__battleLab.state().game.battle.presentation.enemyHp)).toBe(180);
});
