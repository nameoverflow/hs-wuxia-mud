import assert from 'node:assert/strict';
import { createServer } from 'vite';

const server = await createServer({ server: { middlewareMode: true }, appType: 'custom', logLevel: 'error' });
let passed = 0;
const check = (name, test) => { test(); passed++; console.log(`✓ ${name}`); };
try {
  const { resolveCombatTimeline } = await server.ssrLoadModule('/src/battle/animationResolver.ts');
  const { battleActions, idleVisualForStyle } = await server.ssrLoadModule('/src/battle/battleActionCatalog.ts');
  const { sampleBattleScene, sideHome } = await server.ssrLoadModule('/src/battle/battleDirector.ts');
  const { BattleClock } = await server.ssrLoadModule('/src/battle/battleClock.ts');
  const make = (action, result = 'hit', durationMs = action.durationMs) => resolveCombatTimeline({
    kind: 'normal', message: { kind: 'script', text: action.label }, result, damage: result === 'hit' ? 18 : null, heal: null,
    visual: { actionId: action.id, durationMs }
  }, 1, 'player', 'enemy', action.label, 'female', 'male', action.style, 'sword');
  const sample = (timeline, t, reduced = false) => sampleBattleScene(timeline, t, idleVisualForStyle(timeline.actor.visual.style, 'female'), idleVisualForStyle('sword', 'male'), reduced);
  const attacks = Object.values(battleActions).filter(a => ['approach', 'lunge', 'drive'].includes(a.actorMotion));

  const svgPoseModule = await server.ssrLoadModule('/src/battle/svgBattlePose.ts');
  const { sampleSvgPose, SVG_SWORD_LENGTH } = svgPoseModule;
  const { reachPointAt, actorOffsetAt } = await server.ssrLoadModule('/src/battle/battleTiming.ts');
  check('SVG contacts match manifest reach and hit stop freezes articulated poses', () => {
    for (const action of attacks) {
      const timeline = make(action);
      const pose = t => sampleSvgPose(timeline, 'player', t, action.style, action.frames[action.impactFrame].frameId);
      const contact = pose(timeline.impactAtMs);
      const pin = reachPointAt(timeline, timeline.impactAtMs);
      const limb = pin === 'foot' ? contact.foot : contact.hand;
      const blade = pin === 'blade' ? SVG_SWORD_LENGTH : 0;
      // 身法轨道挪动根节点时，姿势里的接触点会反向补偿；这里按世界坐标核对。
      const offset = actorOffsetAt(timeline, timeline.impactAtMs);
      assert.ok(Math.abs(offset.x + limb[0] + blade * Math.cos(contact.blade * Math.PI / 180) - timeline.choreography.reach) < 0.001, action.id);
      assert.ok(Math.abs(offset.y + limb[1] + blade * Math.sin(contact.blade * Math.PI / 180) - (timeline.choreography.contactY - 176)) < 0.001, action.id);
      assert.deepEqual(pose(timeline.impactAtMs + timeline.choreography.hitStopMs - 1), contact);
      const before = pose(timeline.impactAtMs - 0.001);
      assert.ok(Math.abs(before.hand[0] - contact.hand[0]) < 0.1, 'contact must be continuous');
      assert.deepEqual(pose(timeline.durationMs), pose(0));
    }
  });

  const { sampleAttackTrail } = await server.ssrLoadModule('/src/battle/svgAttackTrail.ts');
  check('weapon trails end at contact, freeze with hit stop and respect reduced motion', () => {
    for (const action of attacks) {
      const timeline = make(action);
      const player = idleVisualForStyle(action.style, 'female');
      const enemy = idleVisualForStyle('sword', 'male');
      const trace = sampleAttackTrail(timeline, timeline.impactAtMs, player, enemy);
      const last = trace.points.split(' ').at(-1).split(',').map(Number);
      assert.ok(Math.abs(last[0] - sideHome('enemy')) < 0.001, action.id);
      assert.ok(Math.abs(last[1] - (timeline.choreography.contactY - 176)) < 0.001, action.id);
      assert.deepEqual(sampleAttackTrail(timeline, timeline.impactAtMs + timeline.choreography.hitStopMs - 1, player, enemy), trace);
      assert.deepEqual(sampleAttackTrail(timeline, timeline.impactAtMs, player, enemy, true), { points: '', echoes: [] });
      const release = timeline.choreography.launchAtMs;
      assert.ok(Math.abs(sample(timeline, release - 0.001).player.y - sample(timeline, release).player.y) < 0.01);
    }
  });

  check('every attack preserves ready → preparation → contact → ready', () => {
    for (const action of attacks) {
      const timeline = make(action);
      const before = sample(timeline, timeline.impactAtMs - 1);
      assert.equal(before.enemy.frameId, 'sword_ready', `${action.id}: early hurt`);
      assert.equal(before.burst, 0);
      const contact = sample(timeline, timeline.impactAtMs);
      assert.equal(contact.enemy.frameId, 'sword_hurt');
      assert.equal(contact.player.frameId, action.frames[action.impactFrame].frameId);
      assert.ok(Math.abs(sideHome('player') + contact.player.x + action.choreography.reach - sideHome('enemy')) < 0.001, `${action.id}: contact reach`);
      const end = sample(timeline, timeline.durationMs);
      assert.equal(end.player.x, 0);
      assert.equal(end.enemy.x, 0);
      assert.equal(end.player.frameId, action.style === 'sword' ? 'sword_ready' : 'idle');
      assert.equal(end.enemy.frameId, 'sword_ready');
      assert.equal(end.trail + end.burst + end.guard + end.textAlpha, 0);
    }
  });

  check('server duration changes scale dash and broad attack together', () => {
    for (const action of attacks) for (const factor of [0.6, 1.8]) {
      const timeline = make(action, 'hit', Math.round(action.durationMs * factor));
      assert.equal(timeline.durationMs - timeline.actor.actionDelayMs, Math.round(action.durationMs * factor));
      assert.ok(timeline.actor.actionDelayMs > 0);
      assert.equal(sample(timeline, timeline.impactAtMs + 1).player.frameId, action.frames[action.impactFrame].frameId);
    }
  });

  check('dodge moves before contact and parry stays planted', () => {
    const dodge = make(attacks[0], 'dodge');
    const parry = make(attacks[0], 'parry');
    assert.ok(sample(dodge, dodge.impactAtMs - 30).enemy.x > 10);
    assert.equal(sample(dodge, dodge.impactAtMs).burst, 0);
    assert.ok(sample(parry, parry.impactAtMs + 80).enemy.x <= 4);
    assert.equal(sample(parry, parry.impactAtMs).guard, 1);
  });

  check('hit stop freezes the entire moving composition', () => {
    const timeline = make(battleActions['rig.fist.heavy_a']);
    const first = sample(timeline, timeline.impactAtMs);
    const held = sample(timeline, timeline.impactAtMs + timeline.choreography.hitStopMs - 1);
    for (const field of ['player', 'enemy', 'cameraX', 'cameraScale', 'burst', 'trail', 'textLift']) assert.deepEqual(held[field], first[field]);
    // 写意硬切：受击在接触帧直接到位，定格期间保持，而不是渐进加速。
    assert.ok(Math.abs(first.enemy.x) > 30, 'heavy hit must snap the defender back at contact');
    assert.equal(first.invert, 1, 'hit stop flashes the stage');
    assert.equal(sample(timeline, timeline.impactAtMs - 1).invert, 0, 'no flash before contact');
    assert.equal(sample(timeline, timeline.impactAtMs + timeline.choreography.hitStopMs + 1).invert, 0, 'flash ends with the hold');
  });

  check('attacks cut between key poses instead of blending', () => {
    for (const action of attacks) {
      const timeline = make(action);
      const pose = t => sampleSvgPose(timeline, 'player', t, action.style, action.frames[action.impactFrame].frameId);
      const c = timeline.choreography;
      // 出招一帧到位：刚过起手标记就已是接触姿势。
      assert.deepEqual(pose(c.launchAtMs + 1), pose(timeline.impactAtMs), `${action.id}: strike must cut in`);
      // 位移是换位不是滑行：入场前半段原地，过中点直接落位。
      const arrival = timeline.actor.actionDelayMs;
      assert.equal(sample(timeline, arrival * 0.4).player.x, 0, `${action.id}: plant before the cut`);
      assert.equal(sample(timeline, arrival * 0.6).player.x, sample(timeline, arrival).player.x, `${action.id}: land in one cut`);
    }
  });

  check('parry stops the weapon short of the defender', () => {
    for (const action of attacks) {
      const hit = make(action, 'hit');
      const parry = make(action, 'parry');
      const frameId = action.frames[action.impactFrame].frameId;
      const reachOf = timeline => {
        const pose = sampleSvgPose(timeline, 'player', timeline.impactAtMs, action.style, frameId);
        const pin = reachPointAt(timeline, timeline.impactAtMs);
        const limb = pin === 'foot' ? pose.foot : pose.hand;
        const blade = pin === 'blade' ? SVG_SWORD_LENGTH * Math.cos(pose.blade * Math.PI / 180) : 0;
        return limb[0] + blade;
      };
      assert.ok(reachOf(parry) < reachOf(hit) - 15, `${action.id}: parried weapon must not pierce the body`);
    }
  });

  check('reduced motion retains hit facts while removing movement and trails', () => {
    const timeline = make(battleActions['rig.fist.heavy_a']);
    const frame = sample(timeline, timeline.impactAtMs + 60, true);
    assert.equal(frame.player.x + frame.enemy.x + frame.cameraX + frame.trail + frame.ghost + frame.textLift, 0);
    assert.equal(frame.cameraScale, 1);
    assert.equal(frame.enemy.frameId, 'sword_hurt');
    assert.ok(frame.textAlpha > 0);
  });

  check('multi-hit actions split damage, pin every contact and freeze each hold', () => {
    const combo = battleActions['rig.fist.combo_a'];
    const timeline = make(combo);
    assert.equal(timeline.hits.length, 3);
    assert.deepEqual(timeline.hits.map(h => h.damage), [5, 4, 9]);
    assert.equal(timeline.hits.reduce((sum, h) => sum + h.damage, 0), 18);
    assert.deepEqual(timeline.hits.map(h => h.floatText), ['-5', '-4', '-9']);
    const pinned = timeline.actor.poseKeys.filter(k => k.pin);
    timeline.hits.forEach((hit, i) => {
      const pose = t => sampleSvgPose(timeline, 'player', t, combo.style, 'idle');
      const key = pinned[i];
      const limb = key.pin === 'foot' ? pose(hit.atMs).foot : pose(hit.atMs).hand;
      assert.deepEqual(limb, [timeline.choreography.reach, (key.contactY ?? timeline.choreography.contactY) - 176], `hit ${i} contact`);
      const first = sample(timeline, hit.atMs);
      const held = sample(timeline, hit.atMs + hit.hitStopMs - 1);
      for (const field of ['player', 'enemy', 'cameraX', 'burst', 'trail']) assert.deepEqual(held[field], first[field], `hit ${i} ${field}`);
      assert.deepEqual(pose(hit.atMs + hit.hitStopMs - 1), pose(hit.atMs));
      assert.equal(first.hitIndex, i);
      assert.equal(first.burst, 1, `hit ${i} re-bursts`);
      assert.equal(first.invert, 1, `hit ${i} flashes`);
      assert.equal(sample(timeline, hit.atMs + hit.hitStopMs + 1).invert, 0);
    });
    assert.equal(sample(timeline, timeline.hits[1].atMs - 1).hitIndex, 0);
  });

  check('staging overrides drive reaction, camera and pose per action and per hit', () => {
    const palm = make(battleActions['rig.fist.palm_knockback_a']);
    const at = sample(palm, palm.impactAtMs);
    assert.equal(at.enemy.x, 120, 'knockback push');
    assert.equal(at.enemy.y, -14, 'knockback lift');
    assert.equal(at.enemy.angle, 26, 'knockback tilt');
    const knocked = sampleSvgPose(palm, 'enemy', palm.impactAtMs, 'sword', at.enemy.frameId);
    const { svgPose } = svgPoseModule;
    assert.deepEqual(knocked, svgPose('knocked_back', 'sword'), 'reaction pose override');
    // 同一招里最后一段更重：镜头砸得更狠，受击退得更远。
    const combo = make(battleActions['rig.fist.combo_a']);
    const first = sample(combo, combo.hits[0].atMs), last = sample(combo, combo.hits[2].atMs);
    assert.ok(last.cameraX > first.cameraX, 'heavier final hit kicks harder');
    assert.equal(last.enemy.x, 70);
  });

  check('offset tracks lift the actor while the pinned limb still lands on the target', () => {
    const leap = make(battleActions['rig.fist.leap_kick_a']);
    const hit = sample(leap, leap.impactAtMs);
    assert.equal(hit.player.y, -44);
    assert.ok(hit.player.angle < 0);
    assert.equal(sample(leap, leap.durationMs).player.y, 0, 'lands again');
  });

  check('ranged actions strike from home and keep a continuous dodge across hits', () => {
    const qi = make(battleActions['rig.sword.qi_wave_a']);
    for (let t = 0; t <= qi.durationMs; t += 20) assert.equal(sample(qi, t).player.x, 0, 'no travel');
    assert.equal(qi.actor.actionDelayMs, 0);
    const at = sample(qi, qi.impactAtMs);
    assert.equal(at.force, 1);
    assert.equal(at.contact.y, qi.choreography.contactY - 176);
    assert.equal(at.burst, 1);
    const combo = make(battleActions['rig.fist.combo_a'], 'dodge');
    for (const hit of combo.hits.slice(1)) assert.equal(sample(combo, hit.atMs - 20).enemy.x, 52, 'dodge must not reset between hits');
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
