<script lang="ts">
  import { sendAction, skillAvailability, type GameState } from "../game";
  import { translate } from "../i18n";
  import type { ActiveSkillSummary } from "../protocol";

  export let state: GameState;

  function perform(skill: ActiveSkillSummary) {
    const availability = skillAvailability(state, skill);
    if (!availability.ready) return;
    sendAction({ perform: skill.activeSkillSummaryId });
  }
</script>

<section class="active-skill-panel">
  <div class="section-heading">
    <h2>{translate(state.locale, "panel.active_skills")}</h2>
    <span>{state.battle.activeSkills.length}</span>
  </div>

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
</section>
