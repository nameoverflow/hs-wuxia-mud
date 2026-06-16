<script lang="ts">
  import { formatArtType, sendAction, type GameState } from "../game";
  import { translate } from "../i18n";

  export let state: GameState;
</script>

<section class="arts-panel shell-panel">
  <div class="section-heading">
    <h2>{translate(state.locale, "panel.arts")}</h2>
    <span>{state.arts.length}</span>
  </div>

  {#if state.arts.length === 0}
    <p class="empty">{translate(state.locale, "ui.none")}</p>
  {:else}
    <div class="art-list">
      {#each state.arts as art}
        <article class:foundation={art.artSummaryIsFoundation}>
          <div class="art-head">
            <strong>{art.artSummaryName}</strong>
            <span>{formatArtType(state.locale, art.artSummaryType)}</span>
          </div>
          <p>{translate(state.locale, "art.level", { level: art.artSummaryLevel, max: art.artSummaryMaxLevel })}</p>
          {#if art.artSummaryUnlockedAttackMoves.length || art.artSummaryUnlockedActiveSkills.length}
            <small>{[...art.artSummaryUnlockedAttackMoves, ...art.artSummaryUnlockedActiveSkills].join(" / ")}</small>
          {/if}
          {#if !art.artSummaryIsFoundation && art.artSummaryLevel < art.artSummaryMaxLevel}
            <button type="button" disabled={!state.connected || state.battle.active} on:click={() => sendAction({ train: art.artSummaryId })}>
              {translate(state.locale, "action.train")}
            </button>
          {/if}
        </article>
      {/each}
    </div>
  {/if}
</section>
