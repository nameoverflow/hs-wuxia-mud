<script lang="ts">
  import { onMount } from "svelte";
  import type { GameState } from "../game";
  import { combatStyleFromSnapshot, idleVisualForStyle, visualProfileFromGender } from "../battle/battleActionCatalog";
  import { battlePlayback } from "../battle/battleClock";
  import { sampleBattleScene, sideHome } from "../battle/battleDirector";
  import SvgBattleActor from "./SvgBattleActor.svelte";
  import { sampleAttackTrail } from "../battle/svgAttackTrail";
  import { sampleSvgPose } from "../battle/svgBattlePose";
  import { stageArt } from "../battle/stageAssets";
  import type { BattleSide } from "../battle/animationTypes";

  export let state: GameState;
  // The lab can sample a frozen frame without inventing another playback path.
  export let previewTime: number | null = null;
  let reducedMotion = false;
  const sides: BattleSide[] = ["player", "enemy"];
  $: timeline = state.battle.animation.activeTimeline;
  $: playerVisual = timeline?.actor.side === "player" ? timeline.actor.visual : timeline?.target.side === "player" ? timeline.target.visual : null;
  $: enemyVisual = timeline?.actor.side === "enemy" ? timeline.actor.visual : timeline?.target.side === "enemy" ? timeline.target.visual : null;
  $: playerIdle = idleVisualForStyle(playerVisual?.style ?? combatStyleFromSnapshot(state.battle.player?.combatantSnapshotCombatStyle), playerVisual?.profile ?? visualProfileFromGender(state.battle.player?.combatantSnapshotGender || state.stats.gender));
  $: enemyIdle = idleVisualForStyle(enemyVisual?.style ?? combatStyleFromSnapshot(state.battle.enemy?.combatantSnapshotCombatStyle), enemyVisual?.profile ?? visualProfileFromGender(state.battle.enemy?.combatantSnapshotGender));
  $: elapsed = previewTime ?? ($battlePlayback.id === timeline?.id ? $battlePlayback.elapsedMs : 0);
  $: scene = sampleBattleScene(timeline, elapsed, playerIdle, enemyIdle, reducedMotion);
  $: stroke = sampleAttackTrail(timeline, elapsed, playerIdle, enemyIdle, reducedMotion);

  $: direction = timeline?.actor.side === "enemy" ? -1 : 1;
  $: targetSide = timeline?.target.side ?? "enemy";
  $: resultWord = timeline?.result === "dodge" ? "闪" : timeline?.result === "parry" ? "架" : timeline?.heal ? "息" : "";

  onMount(() => {
    const media = window.matchMedia("(prefers-reduced-motion: reduce)");
    const update = () => { reducedMotion = media.matches; };
    update();
    media.addEventListener("change", update);
    return () => media.removeEventListener("change", update);
  });

</script>

<div class="silhouette-stage" data-renderer="svg" data-phase={scene.phase} data-event-id={timeline?.id ?? "idle"} aria-hidden="true">
  <img class="stage-backdrop" src={stageArt.backdrop} alt="" draggable="false" />
  <div class="stage-shade" style:opacity={0.26 + scene.shade}></div>
  <div class="stage-caption">
    <span class="stage-location">{state.room.name || (state.locale === "zh" ? "江湖 · 交锋" : "Jianghu · Duel")}</span>
    <span class="stage-action" style:opacity={timeline && scene.phase !== "idle" ? 0.9 : 0}>{timeline?.label ?? ""}</span>
  </div>
  <div class="duel-world" style:transform={`translateX(${scene.cameraX}px) scale(${scene.cameraScale})`}>
    {#if scene.trail > 0}
      {#each stroke.echoes as echo, i}
        <div class="figure-home attack-echo" class:enemy={timeline?.actor.side === 'enemy'} style:opacity={scene.trail * (i === 0 ? 0.08 : 0.16)} style:transform={`translate(${echo.x}px,${echo.figure.y}px) scaleX(${direction})`}>
          <SvgBattleActor pose={echo.pose} style={echo.figure.visual.style} profile={echo.figure.visual.profile} />
        </div>
      {/each}
    {/if}
    {#each sides as side}
      {@const figure = scene[side]}
      {@const mirror = side === "enemy" ? -1 : 1}
      {@const pose = sampleSvgPose(timeline, side, elapsed, figure.visual.style, figure.frameId, reducedMotion)}
      <div class="figure-home" class:enemy={side === "enemy"} style:transform={`translateX(${sideHome(side)}px)`}>
        {#if scene.ghost > 0 && side === targetSide}
          <div class="figure-pose ghost" style:opacity={scene.ghost} style:transform={`scaleX(${mirror})`}>
            <SvgBattleActor {pose} style={figure.visual.style} profile={figure.visual.profile} />
          </div>
        {/if}
        <div class="figure-pose" data-side={side} data-frame={figure.frameId} style:opacity={figure.alpha} style:transform={`translate(${figure.x}px, ${figure.y}px) rotate(${figure.angle}deg) scaleX(${mirror})`}>
          <SvgBattleActor {pose} style={figure.visual.style} profile={figure.visual.profile} flash={scene.phase === "impact" ? figure.flash : 0} />
        </div>
      </div>
    {/each}
    <svg class="vector-effects" viewBox="-240 -200 480 240" aria-hidden="true">
      <g opacity={scene.trail} fill="none" stroke-linecap="round" stroke-linejoin="round">
        <polyline points={stroke.points} stroke="#ead9ad" stroke-width="12" opacity="0.09" />
        <polyline points={stroke.points} stroke="#f7e3b0" stroke-width="4" opacity="0.3" />
        <polyline points={stroke.points} stroke="#fff1cb" stroke-width="1.5" opacity="0.9" />
      </g>
      <g transform={`translate(${scene.contact.x} ${scene.contact.y})`} fill="none" stroke-linecap="round">
        <g opacity={scene.burst} stroke="#ffe4b6" stroke-width="2">
          <path d="M-17,-14 L-7,-6 M6,5 L21,17 M-20,5 L-9,2 M8,-3 L25,-8 M1,-12 L4,-24 M-2,10 L-5,23" />
          <path d="M-6,0 L0,-6 6,0 0,6 Z" fill="#fff0cc" stroke="none" />
        </g>
        <g opacity={scene.guard} stroke="#bce5db" stroke-width="3"><path d="M-9,-32 Q-31,0 -9,32 M-3,-23 Q-18,0 -3,23" /></g>
      </g>
      <g transform={`translate(${sideHome(targetSide)} -66)`} opacity={scene.aura} fill="none" stroke="#aed3b3">
        <ellipse rx="38" ry="53" stroke-width="1.5" /><path d="M-25,30 Q-46,-14 -16,-42 M25,-30 Q46,14 16,42" stroke-width="3" />
      </g>
    </svg>
    <div class="result-word" class:healing={!!timeline?.heal} style:opacity={scene.textAlpha} style:transform={`translate(${sideHome(targetSide) + (targetSide === "enemy" ? 35 : -35)}px, ${-147 - scene.textLift}px)`}>
      {#if resultWord}<b>{resultWord}</b>{/if}
      {#if timeline?.floatText && timeline.floatText !== resultWord}<span>{timeline.floatText}</span>{/if}
    </div>
  </div>
  {#if timeline?.kind === "settlement"}
    <div class="settlement" style:opacity={scene.resultAlpha}>
      <strong>{timeline.label}</strong>
      <span>{timeline.text}</span>
    </div>
  {/if}
  <div class="stage-baseline"><span>{state.battle.player?.combatantSnapshotName || state.username}</span><i>·</i><span>{state.battle.enemy?.combatantSnapshotName || ""}</span></div>
</div>

<style>
  .silhouette-stage { position: absolute; inset: 0; overflow: hidden; isolation: isolate; background: #101916; container-type: inline-size; }
  .stage-backdrop { position: absolute; width: 100%; height: 100%; object-fit: cover; object-position: center 66%; opacity: 0.8; }
  .stage-shade { position: absolute; inset: 0; background: #0b1715; pointer-events: none; }
  .stage-caption { position: absolute; inset: 15px 18px auto; display: flex; justify-content: space-between; gap: 12px; font: 12px/1.4 "Songti SC", "Noto Serif CJK SC", serif; color: #d4cfb8; letter-spacing: 0.14em; }
  .stage-location { opacity: 0.65; }
  .stage-action { font-size: 15px; color: #efe0af; text-align: right; }
  .duel-world { position: absolute; left: 50%; top: 81%; width: 0; height: 0; transform-origin: center -65px; zoom: 0.88; }
  .figure-home { position: absolute; width: 0; height: 0; color: #e8d5a3; }
  .figure-home.enemy { color: #a0bdb3; }
  .figure-pose { position: absolute; width: 0; height: 0; transform-origin: 0 0; }
  .ghost { color: #b9d4cd; }
  .vector-effects { position: absolute; left: -240px; top: -200px; width: 480px; height: 240px; overflow: visible; pointer-events: none; }
  .result-word { position: absolute; white-space: nowrap; display: flex; gap: 5px; align-items: center; justify-content: center; color: #ffe1ac; text-shadow: 0 2px 5px #07110e; font: 700 20px/1 "Songti SC", serif; }
  .result-word b { font-size: 26px; font-weight: 600; }
  .result-word.healing { color: #bae1c6; }
  .settlement { position: absolute; inset: 0; display: flex; align-items: center; justify-content: center; flex-direction: column; gap: 10px; background: #08110d66; color: #eddbac; text-shadow: 0 2px 10px #07110e; }
  .settlement strong { font: 64px/1 "STKaiti", "KaiTi", "Songti SC", serif; }
  .settlement span { max-width: 85%; text-align: center; font: 13px/1.7 "Songti SC", serif; }
  .stage-baseline { position: absolute; inset: auto 20px 11px; display: flex; justify-content: center; gap: 13px; color: #c2c8b9; font: 11px/1.4 "Songti SC", serif; letter-spacing: 0.12em; }
  .stage-baseline i { font-style: normal; opacity: 0.4; }
  @container (max-width: 420px) { .duel-world { zoom: 0.65; } .stage-caption { inset: 12px 12px auto; font-size: 10px; } .stage-action { font-size: 12px; } }
  @container (max-width: 320px) { .duel-world { zoom: 0.5; } }
</style>
