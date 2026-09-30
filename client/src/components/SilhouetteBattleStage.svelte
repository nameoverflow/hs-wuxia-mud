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
  import { sampleVfx } from "../battle/battleVfx";
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
  // 多段命中时逐段飘字；尚未接触时沿用整招的字。
  $: hitText = timeline ? (timeline.hits[scene.hitIndex]?.floatText ?? timeline.floatText) : "";
  // 多段时逐段显示结果字：先中后闪就先飘伤害再出“闪”。
  $: hitResult = timeline ? timeline.hits[Math.max(0, scene.hitIndex)].result : null;
  $: resultWord = hitResult === "dodge" ? "闪" : hitResult === "parry" ? "架" : timeline?.heal ? "息" : "";
  // 重招才盖招式名印章；普通招式保持克制。
  $: stampLabel = scene.force >= 1 && timeline && scene.phase !== "idle" ? timeline.label : "";
  $: sprites = sampleVfx(timeline, scene, elapsed, reducedMotion);

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
  <div class="duel-world" style:transform={`translate(${scene.cameraX}px, ${scene.shakeY}px) rotate(${scene.angle}deg) scale(${scene.cameraScale})`}>
    {#if scene.trail > 0}
      {#each stroke.echoes as echo, i}
        <div class="figure-home attack-echo" class:enemy={timeline?.actor.side === 'enemy'} style:opacity={scene.trail * (i === 0 ? 0.1 : 0.2)} style:transform={`translate(${echo.x}px,${echo.figure.y}px) scaleX(${direction})`}>
          <SvgBattleActor pose={echo.pose} style={echo.figure.visual.style} profile={echo.figure.visual.profile} />
        </div>
      {/each}
    {/if}
    {#each sides as side}
      {@const figure = scene[side]}
      {@const mirror = side === "enemy" ? -1 : 1}
      {@const pose = sampleSvgPose(timeline, side, elapsed, figure.visual.style, figure.frameId, reducedMotion)}
      <div class="figure-home" class:enemy={side === "enemy"} style:transform={`translateX(${sideHome(side)}px)`}>
        <!-- 地面阴影留在地上，人离地时变小变淡，冲刺和跃起才读得出高度。 -->
        <div class="ground-shadow" style:transform={`translateX(${figure.x}px) scale(${Math.max(0.55, 1 + figure.y / 60)})`} style:opacity={figure.alpha * Math.max(0.35, 1 + figure.y / 50)}></div>
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
        <polyline points={stroke.points} stroke="#ead9ad" stroke-width="14" opacity="0.1" />
        <polyline points={stroke.points} stroke="#f7e3b0" stroke-width="4.5" opacity="0.34" />
        <polyline points={stroke.points} stroke="#fff1cb" stroke-width="1.5" opacity="0.9" />
      </g>
    </svg>
    {#each sprites as sprite (sprite.key)}
      <!-- 墨迹特效：外层定位到锚点并负责旋转缩放，图自身按攻击方向翻转。 -->
      <div class="ink-anchor" style:transform={`translate(${sprite.x}px, ${sprite.y}px) rotate(${sprite.rotate}deg) scale(${sprite.scale})`}>
        <img class="ink-vfx" src={stageArt[sprite.art]} alt="" draggable="false"
          style:width={`${sprite.size}px`} style:height={`${sprite.size}px`} style:margin={`${-sprite.size / 2}px 0 0 ${-sprite.size / 2}px`}
          style:opacity={sprite.opacity} style:transform={`scaleX(${sprite.flip})`} />
      </div>
    {/each}
    <div class="result-word" class:healing={!!timeline?.heal} style:opacity={scene.textAlpha} style:transform={`translate(${sideHome(targetSide) + (targetSide === "enemy" ? 35 : -35)}px, ${-170 - scene.textLift}px) scale(${scene.textScale})`}>
      {#if resultWord}<b>{resultWord}</b>{/if}
      {#if hitText && hitText !== resultWord}<span>{hitText}</span>{/if}
    </div>
    {#if stampLabel}
      <!-- 招式名印章：重招命中时压在舞台上方。 -->
      <div class="move-stamp" style:opacity={scene.textAlpha} style:transform={`translate(-50%,0) scale(${scene.textScale})`}>{stampLabel}</div>
    {/if}
  </div>
  {#if scene.invert > 0}<div class="stage-invert" style:opacity={scene.invert * 0.22}></div>{/if}
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
  /* 命中定格的反白闪：暗场里要用 screen 才亮得起来，overlay 在暗底上几乎无效。 */
  .stage-invert { position: absolute; inset: 0; background: #fff4dc; mix-blend-mode: screen; pointer-events: none; }
  .stage-caption { position: absolute; inset: 15px 18px auto; display: flex; justify-content: space-between; gap: 12px; font: 12px/1.4 "Songti SC", "Noto Serif CJK SC", serif; color: #d4cfb8; letter-spacing: 0.14em; }
  .stage-location { opacity: 0.65; }
  .stage-action { font-size: 15px; color: #efe0af; text-align: right; }
  .duel-world { position: absolute; left: 50%; top: 81%; width: 0; height: 0; transform-origin: center -65px; zoom: 0.62; }
  .figure-home { position: absolute; width: 0; height: 0; color: #e8d5a3; }
  .figure-home.enemy { color: #a0bdb3; }
  .figure-pose { position: absolute; width: 0; height: 0; transform-origin: 0 0; }
  .ground-shadow { position: absolute; left: -26px; top: -5px; width: 52px; height: 10px; border-radius: 50%; background: #050b09; opacity: 0.55; transform-origin: center; }
  .ghost { color: #b9d4cd; }
  .vector-effects { position: absolute; left: -240px; top: -200px; width: 480px; height: 240px; overflow: visible; pointer-events: none; }
  /* 图以锚点为中心：定位在外层，图自身回退半个尺寸，这样缩放和翻转都以中心为原点。 */
  .ink-anchor { position: absolute; left: 0; top: 0; width: 0; height: 0; pointer-events: none; }
  .ink-vfx { position: absolute; pointer-events: none; transform-origin: center; mix-blend-mode: screen; }
  .result-word { position: absolute; white-space: nowrap; display: flex; gap: 5px; align-items: center; justify-content: center; color: #ffe1ac; text-shadow: 0 2px 5px #07110e; font: 700 30px/1 "Songti SC", serif; }
  .result-word b { font-size: 38px; font-weight: 600; }
  .result-word.healing { color: #bae1c6; }
  .move-stamp { position: absolute; left: 0; top: -196px; white-space: nowrap; font: 600 22px/1 "STKaiti", "KaiTi", "Songti SC", serif; letter-spacing: 0.24em; color: #ffeec2; text-shadow: 0 2px 12px #07110e, 0 0 26px #6d5a2a; }
  .settlement { position: absolute; inset: 0; display: flex; align-items: center; justify-content: center; flex-direction: column; gap: 10px; background: #08110d66; color: #eddbac; text-shadow: 0 2px 10px #07110e; }
  .settlement strong { font: 64px/1 "STKaiti", "KaiTi", "Songti SC", serif; }
  .settlement span { max-width: 85%; text-align: center; font: 13px/1.7 "Songti SC", serif; }
  .stage-baseline { position: absolute; inset: auto 20px 11px; display: flex; justify-content: center; gap: 13px; color: #c2c8b9; font: 11px/1.4 "Songti SC", serif; letter-spacing: 0.12em; }
  .stage-baseline i { font-style: normal; opacity: 0.4; }
  @container (max-width: 420px) { .duel-world { zoom: 0.44; } .stage-caption { inset: 12px 12px auto; font-size: 10px; } .stage-action { font-size: 12px; } .move-stamp { font-size: 16px; } }
  @container (max-width: 320px) { .duel-world { zoom: 0.36; } }
</style>
