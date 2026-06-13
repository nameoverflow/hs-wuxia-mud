<script lang="ts">
  import { formatArtType, sendAction, skillAvailability, type GameState } from "../game";
  import { translate } from "../i18n";
  import type { ActiveSkillSummary } from "../protocol";

  export let state: GameState;

  function perform(skill: ActiveSkillSummary) {
    const availability = skillAvailability(state, skill);
    if (!availability.ready) return;
    sendAction({ perform: skill.activeSkillSummaryId });
  }
</script>

<section class="arts-panel shell-panel">
  <div class="section-heading">
    <h2>{translate(state.locale, "panel.arts")}</h2>
    <span>{state.battle.active ? state.battle.activeSkills.length : state.arts.length}</span>
  </div>

  {#if state.battle.active}
    {#if state.battle.activeSkills.length === 0}
      <p class="empty">{translate(state.locale, "ui.none")}</p>
    {:else}
      <div class="skill-grid">
        {#each state.battle.activeSkills as skill}
          {@const availability = skillAvailability(state, skill)}
          <button
            type="button"
            class:ready={availability.ready}
            class:ultimate={(skill.activeSkillSummaryDamage || 0) >= 100}
            disabled={!availability.ready}
            on:click={() => perform(skill)}
          >
            <span>{skill.activeSkillSummaryName}</span>
            <strong>{availability.label}</strong>
            <small>
              {translate(state.locale, "resource.qi")} {skill.activeSkillSummaryCost}
              · {translate(state.locale, "resource.ap")} {skill.activeSkillSummaryApReq}
            </small>
            {#if skill.activeSkillSummaryDamage}
              <em>伤 {skill.activeSkillSummaryDamage}</em>
            {:else if skill.activeSkillSummaryHeal}
              <em>疗 {skill.activeSkillSummaryHeal}</em>
            {/if}
          </button>
        {/each}
      </div>
    {/if}
  {:else}
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
  {/if}
</section>
