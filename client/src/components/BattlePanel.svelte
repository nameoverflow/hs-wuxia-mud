<script lang="ts">
  import { percent, type GameState } from "../game";
  import { translate } from "../i18n";

  export let state: GameState;

  $: player = state.battle.player;
  $: enemy = state.battle.enemy;

  function toneClass(tone: string) {
    return `micro-meter ${tone}`;
  }
</script>

{#if state.battle.active}
  <section class="battle-panel shell-panel">
    <div class="section-heading battle-heading">
      <h2>{translate(state.locale, "panel.battle")}</h2>
      <span>{enemy?.combatantSnapshotName || "..."}</span>
    </div>

    <div class="duel-grid">
      <div class="combatant player">
        <div class="avatar-mark">侠</div>
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
          <span style={`width: ${percent(player?.combatantSnapshotAp ?? state.stats.ap, 100)}%`}></span>
          <em>{player?.combatantSnapshotAp ?? state.stats.ap}/100</em>
        </div>
      </div>

      <div class="duel-symbol">对</div>

      <div class="combatant enemy">
        <div class="avatar-mark enemy-mark">敌</div>
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
          <span style={`width: ${percent(enemy?.combatantSnapshotAp ?? 0, 100)}%`}></span>
          <em>{enemy?.combatantSnapshotAp ?? 0}/100</em>
        </div>
      </div>
    </div>
  </section>
{/if}
