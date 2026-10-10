<script lang="ts">
  import { artRankTitle, formatArtType, percent, sendAction, trainingLabel, type GameState } from "../game";
  import { translate } from "../i18n";
  import type { ArtSummary } from "../protocol";

  export let state: GameState;

  $: idle = state.connected && !state.battle.active && !state.storyActive && !state.stats.busy;

  const practice = (art: ArtSummary, times: number) =>
    sendAction({ practice: { art: art.artSummaryId, times } });
  const research = (art: ArtSummary) => sendAction({ research: { art: art.artSummaryId, times: 1 } });
  const enable = (art: ArtSummary) => sendAction({ enable: { type: art.artSummaryType, art: art.artSummaryId } });

  const blockerText = (art: ArtSummary) => {
    switch (art.artSummaryBlocker) {
      case "combat_exp":
        return translate(state.locale, "art.blocker.combat_exp", { required: art.artSummaryNextCombatExp ?? "" });
      case "foundation":
        return translate(state.locale, "art.blocker.foundation");
      case "max_level":
        return translate(state.locale, "art.blocker.max_level");
      default:
        return "";
    }
  };
  const canResearch = (art: ArtSummary) =>
    !art.artSummaryIsFoundation && art.artSummaryLevel >= (art.artSummaryResearchFrom ?? Infinity) && art.artSummaryLevel < art.artSummaryMaxLevel;
</script>

<section class="arts-panel shell-panel">
  <div class="section-heading">
    <h2>{translate(state.locale, "panel.arts")}</h2>
    <span>{state.arts.length}</span>
  </div>

  {#if state.stats.busy}
    <div class="training-busy">
      <span>{trainingLabel(state.locale, state.stats.busy, state.arts)}</span>
      <button type="button" disabled={!state.connected} on:click={() => sendAction({ other: "halt" })}>
        {translate(state.locale, "action.halt")}
      </button>
    </div>
  {/if}

  {#if state.arts.length === 0}
    <p class="empty">{translate(state.locale, "ui.none")}</p>
  {:else}
    <div class="art-list">
      {#each state.arts as art}
        <article class:foundation={art.artSummaryIsFoundation} class:enabled={art.artSummaryEnabled}>
          <div class="art-head">
            <strong>{art.artSummaryName}</strong>
            <span>{formatArtType(state.locale, art.artSummaryType)}</span>
          </div>
          <p>
            {artRankTitle(state.locale, art.artSummaryLevel)}
            · {translate(state.locale, "art.level", { level: art.artSummaryLevel, max: art.artSummaryMaxLevel })}
          </p>
          {#if (art.artSummaryNextProgress ?? 0) > 0}
            <div class="art-progress" title={`${art.artSummaryProgress ?? 0}/${art.artSummaryNextProgress}`}>
              <span style={`width: ${percent(art.artSummaryProgress ?? 0, art.artSummaryNextProgress ?? 1)}%`}></span>
            </div>
          {/if}
          {#if blockerText(art)}
            <small class="art-blocker">{blockerText(art)}</small>
          {/if}
          {#if art.artSummaryUnlockedAttackMoves.length || art.artSummaryUnlockedActiveSkills.length}
            <small>{[...art.artSummaryUnlockedAttackMoves, ...art.artSummaryUnlockedActiveSkills].join(" / ")}</small>
          {/if}
          {#if !art.artSummaryIsFoundation}
            <div class="art-actions">
              {#if art.artSummaryBlocker !== "max_level"}
                <button type="button" disabled={!idle} on:click={() => practice(art, 1)}>{translate(state.locale, "action.practice")}</button>
                <button type="button" disabled={!idle} on:click={() => practice(art, 10)}>{translate(state.locale, "action.practice_many")}</button>
              {/if}
              {#if canResearch(art)}
                <button type="button" disabled={!idle} on:click={() => research(art)}>{translate(state.locale, "action.research")}</button>
              {/if}
              {#if art.artSummaryEnabled}
                <span class="art-enabled">{translate(state.locale, "action.enabled")}</span>
              {:else}
                <button type="button" disabled={!state.connected || state.battle.active} on:click={() => enable(art)}>{translate(state.locale, "action.enable")}</button>
              {/if}
            </div>
          {/if}
        </article>
      {/each}
    </div>
  {/if}
</section>
