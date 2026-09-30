<script lang="ts">
  import { onMount } from "svelte";
  import { get } from "svelte/store";
  import { game, clearBattleAnimationQueue } from "../game";
  import BattlePanel from "../components/BattlePanel.svelte";
  import MessageLog from "../components/MessageLog.svelte";
  import { battleActions } from "./battleActionCatalog";
  import { battleClock, battlePlayback } from "./battleClock";
  import { sampleBattleScene } from "./battleDirector";
  import { idleVisualForStyle } from "./battleActionCatalog";
  import { preloadBattleAssets } from "./stageAssets";
  import { playBattleDemo, seedBattle, submitBattleAction } from "./battleFixtures";
  import type { BattleSide, VisualProfile } from "./animationTypes";
  import type { CombatResult } from "../protocol";

  let actionId = "rig.fist.punch_a";
  let outcome: CombatResult = "hit";
  let side: BattleSide = "player";
  let profile: VisualProfile = "female";
  let speed = 1;
  let ready = false;
  const actions = Object.values(battleActions).filter((action) => ["approach", "lunge", "drive", "focus", "ranged"].includes(action.actorMotion) || action.id.startsWith("rig.effect."));

  function playOne() { seedBattle(profile); battleClock.setSpeed(speed); submitBattleAction(actionId, outcome, side); }
  function changeSpeed() { battleClock.setSpeed(speed); }
  function inspectAt(ms: number, reducedMotion = false) {
    const state = get(game);
    const timeline = state.battle.animation.activeTimeline;
    const p = idleVisualForStyle(timeline?.actor.side === "player" ? timeline.actor.visual.style : timeline?.target.visual.style || "fist", profile);
    const e = idleVisualForStyle(timeline?.actor.side === "enemy" ? timeline.actor.visual.style : timeline?.target.visual.style || "sword", "male");
    return sampleBattleScene(timeline, ms, p, e, reducedMotion);
  }

  onMount(() => {
    seedBattle();
    void preloadBattleAssets().then(() => { ready = true; });
    const lab = {
      seed: seedBattle, submit: submitBattleAction, demo: playBattleDemo,
      pause: (paused = true) => battleClock.pause(paused), seek: (ms: number) => battleClock.seek(ms),
      speed: (value: number) => battleClock.setSpeed(value),
      inspectAt,
      state: () => ({ game: get(game), playback: get(battlePlayback) }),
      ready: () => ready
    };
    (window as Window & { __battleLab?: typeof lab }).__battleLab = lab;
    return () => { clearBattleAnimationQueue(); delete (window as Window & { __battleLab?: typeof lab }).__battleLab; };
  });
</script>

<main class="battle-lab">
  <header class="lab-header"><div><p>武侠 MUD · 动作回放</p><h1>剪影交锋</h1></div><a href="/">返回游戏</a></header>
  <p class="lab-intro">蓄势有静，出手有锋。完整交锋包含连击、闪避、招架、调息与胜负结算。</p>
  <div class="lab-layout">
    <section class="lab-preview">
      {#if $game.battle.active}<BattlePanel state={$game} />{:else}<div class="lab-finished">交锋已结束。可以重新播放，或挑选一招细看。</div>{/if}
      <div class="playback-controls">
        <button disabled={!ready} class="primary" on:click={() => { battleClock.setSpeed(speed); playBattleDemo(); }}>播放完整交锋</button>
        <button disabled={!$game.battle.animation.activeTimeline} on:click={() => battleClock.pause(!$battlePlayback.paused)}>{$battlePlayback.paused ? "继续" : "暂停"}</button>
        <label>速度 <select bind:value={speed} on:change={changeSpeed}><option value={0.25}>¼×</option><option value={0.5}>½×</option><option value={1}>1×</option><option value={1.5}>1.5×</option></select></label>
      </div>
      <label class="time-slider">播放位置 <output>{Math.round($battlePlayback.elapsedMs)} / {$battlePlayback.durationMs} ms</output>
        <input aria-label="播放位置" type="range" min="0" max={$battlePlayback.durationMs || 1} value={$battlePlayback.elapsedMs} on:input={(event) => { battleClock.pause(); battleClock.seek(+event.currentTarget.value); }} />
      </label>
    </section>
    <aside class="lab-controls">
      <h2>单招回放</h2>
      <label>招式<select bind:value={actionId}>{#each actions as action}<option value={action.id}>{action.label}</option>{/each}</select></label>
      <label>结果<select bind:value={outcome}><option value="hit">命中</option><option value="dodge">闪避</option><option value="parry">招架</option><option value="effect">效果</option></select></label>
      <label>出招方<select bind:value={side}><option value="player">行者</option><option value="enemy">守擂人</option></select></label>
      <label>行者剪影<select bind:value={profile}><option value="female">束发</option><option value="male">无发饰</option></select></label>
      <button disabled={!ready} on:click={playOne}>播放这一招</button>
      <p>拖动时间轴可逐帧检查。暂停和慢放使用游戏中的同一条时间线。</p>
    </aside>
  </div>
  <div class="lab-log"><MessageLog state={$game}/></div>
</main>

<style>
  .battle-lab { width: min(1040px, calc(100% - 40px)); margin: 40px auto; color: #d9decf; }
  .lab-header { display: flex; align-items: center; justify-content: space-between; gap: 20px; }
  .lab-header p { color: #8a9d90; font-size: 12px; letter-spacing: 0.15em; margin: 0 0 10px; }
  h1 { font: 38px/1.2 "STKaiti", "KaiTi", "Songti SC", serif; letter-spacing: 0.13em; margin: 0; color: #e4d3a7; }
  .lab-header a { color: #a5b4a8; font-size: 12px; }
  .lab-intro { margin: 18px 0 28px; font-size: 13px; color: #9ea99c; line-height: 1.8; }
  .lab-layout { display: grid; grid-template-columns: minmax(0, 1fr) 205px; gap: 24px; }
  .lab-preview { min-width: 0; }
  .lab-controls { display: flex; flex-direction: column; gap: 13px; padding: 18px; border: 1px solid #33463b; background: #121c17; }
  h2 { margin: 0 0 6px; font-size: 14px; font-weight: 500; }
  label { display: flex; gap: 7px; flex-direction: column; color: #acb7a7; font-size: 11px; }
  select { color: #d4dac9; background: #18241e; border: 1px solid #425346; padding: 8px; min-width: 0; }
  button { color: #d6dccd; background: #26382e; border: 1px solid #50634f; padding: 8px 13px; font-size: 12px; }
  button.primary { color: #f1dfb3; border-color: #807350; }
  .playback-controls { display: flex; align-items: end; gap: 9px; margin-top: 16px; flex-wrap: wrap; }
  .playback-controls label { margin-left: auto; }
  .playback-controls select { padding: 7px; }
  .time-slider { display: grid; grid-template-columns: 1fr auto; margin: 18px 0; }
  .time-slider input { grid-column: 1 / -1; width: 100%; accent-color: #baa777; }
  .lab-controls p { font-size: 11px; line-height: 1.8; color: #8f9f91; }
  .lab-finished { min-height: 260px; display: grid; place-content: center; background: #14231b; color: #caba96; padding: 24px; font-size: 13px; }
  .lab-log { margin-top: 20px; }
  .lab-log :global(.message-log) { height: 160px; }
  @media (max-width: 700px) { .battle-lab { width: calc(100% - 24px); margin: 24px auto; } .lab-layout { grid-template-columns: 1fr; } .lab-controls { display: grid; grid-template-columns: 1fr 1fr; } .lab-controls h2, .lab-controls p { grid-column: 1 / -1; } h1 { font-size: 30px; } }
</style>
