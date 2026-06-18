<script lang="ts">
  import { tick } from "svelte";
  import { percent, type GameState } from "../game";
  import { translate } from "../i18n";
  import ActiveSkillPanel from "./ActiveSkillPanel.svelte";
  import PixiBattleStage from "./PixiBattleStage.svelte";
  import { apMeterTween, meterTween } from "./meterTween";

  export let state: GameState;

  let panelEl: HTMLElement;
  let wasActive = false;

  $: player = state.battle.player;
  $: enemy = state.battle.enemy;
  $: playerAp = player?.combatantSnapshotAp ?? state.stats.ap;
  $: enemyAp = enemy?.combatantSnapshotAp ?? 0;
  $: playerAgility = player?.combatantSnapshotAgility ?? state.stats.agility;
  $: enemyAgility = enemy?.combatantSnapshotAgility ?? 19;
  $: activeActorSide = state.battle.animation.activeTimeline?.actor.side ?? null;
  $: activeTimelineId = state.battle.animation.activeTimeline?.id ?? null;
  $: playerApSnapKey = activeActorSide === "player" ? activeTimelineId : null;
  $: enemyApSnapKey = activeActorSide === "enemy" ? activeTimelineId : null;
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
</script>

{#if state.battle.active}
  <section class="battle-panel shell-panel" bind:this={panelEl}>
    <div class="section-heading battle-heading">
      <h2>{translate(state.locale, "panel.battle")}</h2>
    </div>

    <div class="battle-stage" aria-hidden="true">
      <PixiBattleStage {state} />
    </div>

    <div class="duel-grid">
      <div class="combatant player">
        <strong>{player?.combatantSnapshotName || state.username || "Player"}</strong>
        <div class={toneClass("hp")}>
          <span use:meterTween={{ value: percent(player?.combatantSnapshotHp ?? state.stats.hp, player?.combatantSnapshotMaxHp ?? state.stats.maxHp) / 100 }}></span>
          <em>{player?.combatantSnapshotHp ?? state.stats.hp}/{player?.combatantSnapshotMaxHp ?? state.stats.maxHp}</em>
        </div>
        <div class={toneClass("qi")}>
          <span use:meterTween={{ value: percent(player?.combatantSnapshotQi ?? state.stats.qi, player?.combatantSnapshotMaxQi ?? state.stats.maxQi) / 100 }}></span>
          <em>{player?.combatantSnapshotQi ?? state.stats.qi}/{player?.combatantSnapshotMaxQi ?? state.stats.maxQi}</em>
        </div>
        <div class={toneClass("ap")} use:apMeterTween={{ value: playerAp, agility: playerAgility, snapKey: playerApSnapKey, resumeAt: state.battle.actionLockUntil }}>
          <span></span>
          <em></em>
        </div>
      </div>

      <div class="combatant enemy">
        <strong>{enemy?.combatantSnapshotName || "Enemy"}</strong>
        <div class={toneClass("hp")}>
          <span use:meterTween={{ value: percent(enemy?.combatantSnapshotHp ?? 0, enemy?.combatantSnapshotMaxHp ?? 1) / 100 }}></span>
          <em>{enemy?.combatantSnapshotHp ?? 0}/{enemy?.combatantSnapshotMaxHp ?? 1}</em>
        </div>
        <div class={toneClass("qi")}>
          <span use:meterTween={{ value: percent(enemy?.combatantSnapshotQi ?? 0, enemy?.combatantSnapshotMaxQi ?? 1) / 100 }}></span>
          <em>{enemy?.combatantSnapshotQi ?? 0}/{enemy?.combatantSnapshotMaxQi ?? 1}</em>
        </div>
        <div class={toneClass("ap")} use:apMeterTween={{ value: enemyAp, agility: enemyAgility, snapKey: enemyApSnapKey, resumeAt: state.battle.actionLockUntil }}>
          <span></span>
          <em></em>
        </div>
      </div>
    </div>

    <ActiveSkillPanel state={state} />
  </section>
{/if}
