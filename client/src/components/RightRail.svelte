<script lang="ts">
  import { formatReward, type GameState } from "../game";
  import { translate } from "../i18n";

  export let state: GameState;
</script>

<aside class="right-rail">
  <section class="shell-panel rail-section">
    <div class="section-heading">
      <h2>{translate(state.locale, "panel.quest")}</h2>
      <span>{state.quests.length}</span>
    </div>
    {#if state.quests.length === 0}
      <p class="empty">{translate(state.locale, "ui.none")}</p>
    {:else}
      <div class="quest-list">
        {#each state.quests as quest}
          <article class:complete={quest.questLogEntryCompleted}>
            <div>
              <strong>{quest.questLogEntryName}</strong>
              <span>{quest.questLogEntryCompleted ? translate(state.locale, "quest.completed") : quest.questLogEntryStage}</span>
            </div>
            {#if quest.questLogEntryObjective}
              <p>{quest.questLogEntryObjective}</p>
            {/if}
            {#if quest.questLogEntryRewards?.length}
              <small>{translate(state.locale, "quest.reward", { value: quest.questLogEntryRewards.map(formatReward).join(", ") })}</small>
            {/if}
          </article>
        {/each}
      </div>
    {/if}
  </section>
</aside>
