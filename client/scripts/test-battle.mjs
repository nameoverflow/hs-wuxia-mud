import assert from 'node:assert/strict';
import { createServer } from 'vite';

const server = await createServer({ server: { middlewareMode: true }, appType: 'custom', logLevel: 'error' });
let passed = 0;
const check = (name, test) => { test(); passed++; console.log(`✓ ${name}`); };
try {
  const { resolveCombatTimeline } = await server.ssrLoadModule('/src/battle/animationResolver.ts');
  const { battleActions, idleVisualForStyle } = await server.ssrLoadModule('/src/battle/battleActionCatalog.ts');
  const { sampleBattleScene } = await server.ssrLoadModule('/src/battle/battleDirector.ts');
  const { BattleClock } = await server.ssrLoadModule('/src/battle/battleClock.ts');
  const make = (action, result = 'hit', durationMs = action.durationMs) => resolveCombatTimeline({
    kind: 'normal', message: { kind: 'script', text: action.label }, result, damage: result === 'hit' ? 18 : null, heal: null,
    visual: { actionId: action.id, durationMs }
  }, 1, 'player', 'enemy', action.label, 'female', 'male', action.style, 'sword');
  const sample = (timeline, t, reduced = false) => sampleBattleScene(timeline, t, idleVisualForStyle(timeline.actor.visual.style, 'female'), idleVisualForStyle('sword', 'male'), reduced);
  const attacks = Object.values(battleActions).filter(a => ['approach', 'lunge', 'drive'].includes(a.actorMotion));

  const { sampleSvgPose, SVG_SWORD_LENGTH } = await server.ssrLoadModule('/src/battle/svgBattlePose.ts');
  const { reachPointAt, actorOffsetAt } = await server.ssrLoadModule('/src/battle/battleTiming.ts');
  // 这里只测"画面和战斗结算对得上"的机制：接触点、时长、分段伤害、时钟回调、减弱动态。
  // 姿势长什么样、位移多少、帧名叫什么属于手感，靠姿势总览和 battle-lab 目测，不写死在测试里。

  check('every contact lands the fist, foot or blade tip on the target', () => {
    for (const action of attacks) {
      const timeline = make(action);
      const pinned = timeline.actor.poseKeys.filter(k => k.pin);
      timeline.hits.forEach((hit, i) => {
        const key = pinned[i];
        const pose = sampleSvgPose(timeline, 'player', hit.atMs, action.style, 'idle');
        const pin = reachPointAt(timeline, hit.atMs);
        const limb = pin === 'foot' ? pose.foot : pose.hand;
        const blade = pin === 'blade' ? SVG_SWORD_LENGTH : 0;
        // 身法轨道挪动根节点时，姿势里的接触点会反向补偿；这里按世界坐标核对。
        const offset = actorOffsetAt(timeline, hit.atMs);
        const x = offset.x + limb[0] + blade * Math.cos(pose.blade * Math.PI / 180);
        const y = offset.y + limb[1] + blade * Math.sin(pose.blade * Math.PI / 180);
        assert.ok(Math.abs(x - (key.reach ?? timeline.choreography.reach)) < 1e-6, `${action.id} hit ${i} reach`);
        assert.ok(Math.abs(y - ((key.contactY ?? timeline.choreography.contactY) - 176)) < 1e-6, `${action.id} hit ${i} height`);
      });
    }
  });

  check('server durations scale the whole presentation and the dash', () => {
    for (const action of attacks) for (const factor of [0.6, 1.8]) {
      const timeline = make(action, 'hit', Math.round(action.durationMs * factor));
      assert.equal(timeline.durationMs - timeline.actor.actionDelayMs, Math.round(action.durationMs * factor));
      assert.ok(timeline.actor.actionDelayMs > 0);
      assert.ok(timeline.hits.every((hit) => hit.atMs > timeline.actor.actionDelayMs && hit.atMs < timeline.durationMs));
    }
  });

  check('multi-hit damage splits by share and sums to the server total', () => {
    const timeline = make(battleActions['rig.fist.combo_a']);
    assert.deepEqual(timeline.hits.map(h => h.damage), [5, 4, 9]);
    assert.deepEqual(timeline.hits.map(h => h.floatText), ['-5', '-4', '-9']);
  });

  check('server per-hit outcomes and animation params reach the timeline', () => {
    const combo = battleActions['rig.fist.combo_a'];
    const timeline = resolveCombatTimeline({
      kind: 'normal', message: { kind: 'script', text: combo.label }, result: 'hit', damage: 14, heal: null,
      hits: [{ result: 'hit', damage: 5, heal: null }, { result: 'dodge', damage: 0, heal: null }, { result: 'hit', damage: 9, heal: null }],
      visual: { actionId: combo.id, durationMs: combo.durationMs }
    }, 1, 'player', 'enemy', combo.label, 'female', 'male', 'fist', 'sword');
    assert.deepEqual(timeline.hits.map(h => h.result), ['hit', 'dodge', 'hit']);
    assert.deepEqual(timeline.hits.map(h => h.damage), [5, 0, 9]);
    assert.equal(sample(timeline, timeline.hits[1].atMs).burst, 0, 'no damage spark on a dodged hit');

    const punch = battleActions['rig.fist.punch_a'];
    const restyled = resolveCombatTimeline({
      kind: 'normal', message: { kind: 'script', text: '一段很长的招式描述文字超过十二个字' }, result: 'hit', damage: 10, heal: null,
      visual: { actionId: punch.id, durationMs: punch.durationMs, params: { label: '崩拳', vfxArt: { trail: 'rising', impact: 'nope' }, staging: { camera: { kick: 20 } } } }
    }, 2, 'player', 'enemy', 'x', 'female', 'male', 'fist', 'sword');
    assert.equal(restyled.label, '崩拳');
    assert.equal(restyled.vfx.find(v => v.kind === 'trail').art, 'rising');
    assert.equal(restyled.vfx.find(v => v.kind === 'impact').art, 'impact', 'unknown art names are ignored');
    assert.equal(restyled.hits[0].staging.camera.kick, 20);
  });

  check('reduced motion keeps the hit but removes movement', () => {
    const timeline = make(battleActions['rig.fist.heavy_a']);
    const frame = sample(timeline, timeline.impactAtMs + 60, true);
    assert.equal(frame.player.x + frame.enemy.x + frame.cameraX + frame.trail + frame.ghost + frame.textLift, 0);
    assert.equal(frame.cameraScale, 1);
    assert.ok(frame.textAlpha > 0);
  });

  function driver() {
    let now = 0, serial = 0;
    const callbacks = new Map();
    return {
      api: { now: () => now, request: cb => { callbacks.set(++serial, cb); return serial; }, cancel: id => callbacks.delete(id) },
      step: ms => { now += ms; const pending = [...callbacks.values()]; callbacks.clear(); pending.forEach(cb => cb(now)); }
    };
  }

  check('consecutive identical actions receive independent play cursors and one impact each', () => {
    const d = driver(), clock = new BattleClock(d.api), timeline = make(attacks[0]);
    let impacts = 0, completions = 0, current;
    clock.state.subscribe(value => current = value);
    const play = id => clock.play({ ...timeline, id }, () => impacts++, () => completions++);
    play(1); d.step(timeline.durationMs);
    play(2); assert.equal(current.elapsedMs, 0); assert.equal(current.id, 2);
    d.step(timeline.impactAtMs - 1); assert.equal(impacts, 1);
    d.step(1); assert.equal(impacts, 2);
    d.step(timeline.durationMs); assert.equal(completions, 2);
  });

  check('pause, hidden-tab suspension and cancellation preserve event ordering', () => {
    const d = driver(), clock = new BattleClock(d.api), timeline = make(attacks[0]);
    let impacts = 0, completed = false, current;
    clock.state.subscribe(v => current = v);
    clock.play(timeline, () => impacts++, () => completed = true);
    d.step(50); clock.pause(); d.step(1000); assert.equal(current.elapsedMs, 50);
    clock.suspend(true); clock.pause(false); d.step(1000); assert.equal(current.elapsedMs, 50);
    clock.suspend(false); d.step(timeline.impactAtMs - 50); assert.equal(impacts, 1);
    clock.cancel(); d.step(10000); assert.equal(completed, false); assert.equal(current.id, null);
  });

  check('the clock fires every hit once, in order, even after scrubbing past them', () => {
    const d = driver(), clock = new BattleClock(d.api), timeline = make(battleActions['rig.fist.combo_a']);
    const fired = [];
    clock.play(timeline, i => fired.push(i), () => {});
    d.step(timeline.hits[0].atMs); assert.deepEqual(fired, [0]);
    clock.pause(); clock.seek(timeline.hits[2].atMs + 1); d.step(100); assert.deepEqual(fired, [0]);
    clock.pause(false); d.step(1); assert.deepEqual(fired, [0, 1, 2]);
    clock.seek(0); d.step(timeline.durationMs); assert.deepEqual(fired, [0, 1, 2]);
  });

  check('scrubbing never repeats an applied impact', () => {
    const d = driver(), clock = new BattleClock(d.api), timeline = make(attacks[0]);
    let impacts = 0;
    clock.play(timeline, () => impacts++, () => {});
    clock.pause(); clock.seek(timeline.impactAtMs + 1); d.step(1000); assert.equal(impacts, 0);
    clock.pause(false); d.step(1); assert.equal(impacts, 1);
    clock.seek(0); d.step(timeline.durationMs); assert.equal(impacts, 1);
  });
  console.log(`${passed} battle behavior checks passed.`);
} finally { await server.close(); }
