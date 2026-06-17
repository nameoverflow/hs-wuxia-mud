<script lang="ts">
  import { onDestroy, tick } from "svelte";
  import { combatStyleFromSnapshot, idleVisualForStyle, visualProfileFromGender } from "../battle/rigActionCatalog";
  import type { ActorVisual } from "../battle/animationTypes";
  import { percent, type BattleSide, type GameState } from "../game";
  import { translate } from "../i18n";
  import ActiveSkillPanel from "./ActiveSkillPanel.svelte";
  import RigActor from "./RigActor.svelte";

  export let state: GameState;

  type BattleCombatant = GameState["battle"]["player"];

  let panelEl: HTMLElement;
  let wasActive = false;
  let displayedPlayerAp = 0;
  let displayedEnemyAp = 0;
  let playerApFrame = 0;
  let enemyApFrame = 0;
  let playerVisual: ActorVisual = idleVisualForStyle("fist", "male");
  let enemyVisual: ActorVisual = idleVisualForStyle("fist", "male");

  $: player = state.battle.player;
  $: enemy = state.battle.enemy;
  $: timeline = state.battle.animation.activeTimeline;
  $: timelineKey = timeline?.id ?? 0;
  $: playerVisual = visualFor("player", timeline, player, enemy, state.stats.gender);
  $: enemyVisual = visualFor("enemy", timeline, player, enemy, state.stats.gender);
  $: smoothAp("player", player?.combatantSnapshotAp ?? state.stats.ap);
  $: smoothAp("enemy", enemy?.combatantSnapshotAp ?? 0);
  $: if (state.battle.active && !wasActive) {
    wasActive = true;
    tick().then(() => {
      const el = panelEl;
      window.setTimeout(() => el?.scrollIntoView({ block: "center", behavior: "auto" }), 50);
    });
  }
  $: if (!state.battle.active && wasActive) {
    wasActive = false;
  }

  function toneClass(tone: string) {
    return `micro-meter ${tone}`;
  }

  function visualFor(
    side: BattleSide,
    currentTimeline: typeof timeline,
    playerCombatant: BattleCombatant,
    enemyCombatant: BattleCombatant,
    playerGender: string
  ): ActorVisual {
    if (!currentTimeline) return idleVisualForSide(side, playerCombatant, enemyCombatant, playerGender);
    if (currentTimeline.actor.side === side) return currentTimeline.actor.visual;
    if (currentTimeline.target.side === side) return currentTimeline.target.visual;
    return idleVisualForSide(side, playerCombatant, enemyCombatant, playerGender);
  }

  function idleVisualForSide(
    side: BattleSide,
    playerCombatant: BattleCombatant,
    enemyCombatant: BattleCombatant,
    playerGender: string
  ): ActorVisual {
    const combatant = side === "player" ? playerCombatant : enemyCombatant;
    const gender = side === "player" ? combatant?.combatantSnapshotGender || playerGender : combatant?.combatantSnapshotGender;
    return idleVisualForStyle(
      combatStyleFromSnapshot(combatant?.combatantSnapshotCombatStyle),
      visualProfileFromGender(gender)
    );
  }

  function actorClass(side: BattleSide) {
    const classes = ["stage-actor", side];
    if (timeline && timeline.kind !== "settlement") {
      if (timeline.actor.side === side && timeline.actor.motion !== "none") classes.push(`motion-${timeline.actor.motion}`);
      if (timeline.target.side === side && timeline.target.reaction !== "none") classes.push(`react-${timeline.target.reaction}`);
    }
    return classes.join(" ");
  }

  function floatClass() {
    if (!timeline) return "impact-float";
    return `impact-float target-${timeline.target.side} ${timeline.result}`;
  }

  function impactText() {
    if (!timeline || timeline.kind === "settlement") return "";
    return timeline.floatText;
  }

  function smoothAp(side: BattleSide, target: number) {
    const current = side === "player" ? displayedPlayerAp : displayedEnemyAp;
    const frame = side === "player" ? playerApFrame : enemyApFrame;
    if (frame) cancelAnimationFrame(frame);
    if (target <= current || Math.abs(target - current) < 1) {
      setDisplayedAp(side, target);
      return;
    }

    const start = current;
    const startedAt = performance.now();
    const duration = 900;
    const nextFrame = (now: number) => {
      const progress = Math.min(1, (now - startedAt) / duration);
      setDisplayedAp(side, start + (target - start) * progress);
      if (progress < 1) {
        setApFrame(side, requestAnimationFrame(nextFrame));
      }
    };
    setApFrame(side, requestAnimationFrame(nextFrame));
  }

  function setDisplayedAp(side: BattleSide, value: number) {
    const clean = Math.max(0, Math.min(100, value));
    if (side === "player") displayedPlayerAp = clean;
    else displayedEnemyAp = clean;
  }

  function setApFrame(side: BattleSide, frame: number) {
    if (side === "player") playerApFrame = frame;
    else enemyApFrame = frame;
  }

  onDestroy(() => {
    if (playerApFrame) cancelAnimationFrame(playerApFrame);
    if (enemyApFrame) cancelAnimationFrame(enemyApFrame);
  });
</script>

{#if state.battle.active}
  <section class="battle-panel shell-panel" bind:this={panelEl}>
    <div class="section-heading battle-heading">
      <h2>{translate(state.locale, "panel.battle")}</h2>
    </div>

    <div class="battle-stage" aria-hidden="true">
      {#key timelineKey}
        <div class="stage-cue" style={`--cue-ms: ${timeline?.durationMs ?? 840}ms`}>
          <div class={actorClass("player")}>
            <RigActor visual={playerVisual} durationMs={timeline?.durationMs ?? 720} />
          </div>
          <div class={actorClass("enemy")}>
            <RigActor visual={enemyVisual} durationMs={timeline?.durationMs ?? 720} />
          </div>
          {#if timeline && timeline.kind !== "settlement"}
            {#each timeline.vfx as vfx (vfx.id)}
              <div class={`stage-vfx ${vfx.kind} ${vfx.variant} ${vfx.side}`}></div>
            {/each}
            <div class={floatClass()}>{impactText()}</div>
          {/if}
          {#if timeline?.kind === "settlement"}
            <div class="settlement-flash">{timeline.text}</div>
          {/if}
        </div>
      {/key}
    </div>

    <div class="duel-grid">
      <div class="combatant player">
        <strong>{player?.combatantSnapshotName || state.username || "Player"}</strong>
        <div class={toneClass("hp")}>
          <span style={`width: ${percent(player?.combatantSnapshotHp ?? state.stats.hp, player?.combatantSnapshotMaxHp ?? state.stats.maxHp)}%`}></span>
          <em>{player?.combatantSnapshotHp ?? state.stats.hp}/{player?.combatantSnapshotMaxHp ?? state.stats.maxHp}</em>
        </div>
        <div class={toneClass("qi")}>
          <span style={`width: ${percent(player?.combatantSnapshotQi ?? state.stats.qi, player?.combatantSnapshotMaxQi ?? state.stats.maxQi)}%`}></span>
          <em>{player?.combatantSnapshotQi ?? state.stats.qi}/{player?.combatantSnapshotMaxQi ?? state.stats.maxQi}</em>
        </div>
        <div class={toneClass("ap")}>
          <span style={`width: ${percent(displayedPlayerAp, 100)}%`}></span>
          <em>{Math.round(displayedPlayerAp)}/100</em>
        </div>
      </div>

      <div class="combatant enemy">
        <strong>{enemy?.combatantSnapshotName || "Enemy"}</strong>
        <div class={toneClass("hp")}>
          <span style={`width: ${percent(enemy?.combatantSnapshotHp ?? 0, enemy?.combatantSnapshotMaxHp ?? 1)}%`}></span>
          <em>{enemy?.combatantSnapshotHp ?? 0}/{enemy?.combatantSnapshotMaxHp ?? 1}</em>
        </div>
        <div class={toneClass("qi")}>
          <span style={`width: ${percent(enemy?.combatantSnapshotQi ?? 0, enemy?.combatantSnapshotMaxQi ?? 1)}%`}></span>
          <em>{enemy?.combatantSnapshotQi ?? 0}/{enemy?.combatantSnapshotMaxQi ?? 1}</em>
        </div>
        <div class={toneClass("ap")}>
          <span style={`width: ${percent(displayedEnemyAp, 100)}%`}></span>
          <em>{Math.round(displayedEnemyAp)}/100</em>
        </div>
      </div>
    </div>

    <ActiveSkillPanel state={state} />
  </section>
{/if}
