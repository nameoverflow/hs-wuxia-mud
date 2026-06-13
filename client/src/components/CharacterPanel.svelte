<script lang="ts">
  import { formatStatus, sendAction, type GameState } from "../game";
  import { translate } from "../i18n";
  import ResourceMeter from "./ResourceMeter.svelte";

  export let state: GameState;

  let activeTab: "status" | "inventory" = "status";

  const requestRefresh = () => {
    sendAction({ other: "view" });
    sendAction({ other: "quests" });
    sendAction({ other: "inventory" });
    sendAction({ other: "arts" });
  };
</script>

<aside class="shell-panel character-panel">
  <div class="panel-title-row">
    <h2>{translate(state.locale, "panel.character")}</h2>
    <button class="ghost-button" type="button" disabled={!state.connected} on:click={requestRefresh}>
      {translate(state.locale, "action.refresh")}
    </button>
  </div>

  <div class="panel-tabs" role="tablist" aria-label="Character panel">
    <button
      type="button"
      role="tab"
      aria-selected={activeTab === "status"}
      class:active={activeTab === "status"}
      on:click={() => activeTab = "status"}
    >
      {translate(state.locale, "panel.character")}
    </button>
    <button
      type="button"
      role="tab"
      aria-selected={activeTab === "inventory"}
      class:active={activeTab === "inventory"}
      on:click={() => activeTab = "inventory"}
    >
      {translate(state.locale, "panel.inventory")}
      <span>{state.inventory.length}</span>
    </button>
  </div>

  {#if activeTab === "status"}
    <dl class="identity-grid">
      <div>
        <dt>{translate(state.locale, "field.name")}</dt>
        <dd>{state.username || "-"}</dd>
      </div>
      <div>
        <dt>{translate(state.locale, "field.location")}</dt>
        <dd>{state.room.name || "-"}</dd>
      </div>
      <div>
        <dt>{translate(state.locale, "field.status")}</dt>
        <dd>{formatStatus(state.locale, state.playerStatus)}</dd>
      </div>
      <div>
        <dt>{translate(state.locale, "field.money")}</dt>
        <dd>{state.money}</dd>
      </div>
    </dl>

    <div class="meter-stack">
      <ResourceMeter label={translate(state.locale, "resource.hp")} value={state.stats.hp} max={state.stats.maxHp} tone="hp" />
      <ResourceMeter label={translate(state.locale, "resource.qi")} value={state.stats.qi} max={state.stats.maxQi} tone="qi" />
    </div>

    <section class="compact-section">
      <h3>{translate(state.locale, "panel.effects")}</h3>
      {#if state.effects.length === 0}
        <span class="empty">{translate(state.locale, "ui.none")}</span>
      {:else}
        <div class="effect-list">
          {#each state.effects as effect}
            <span class:bad={effect.effectSummaryType === "debuff" || effect.effectSummaryType === "dot"}>
              {effect.effectSummaryName || effect.effectSummaryId}
              <small>{Math.ceil(effect.effectSummaryRemaining)}s</small>
            </span>
          {/each}
        </div>
      {/if}
    </section>
  {:else}
    <section class="inventory-tab">
      <div class="inventory-summary">
        <span>{translate(state.locale, "field.money")}</span>
        <strong>{state.money}</strong>
      </div>
      {#if state.inventory.length === 0}
        <p class="empty">{translate(state.locale, "ui.none")}</p>
      {:else}
        <div class="inventory-list">
          {#each state.inventory as item}
            <article>
              <div>
                <strong>{item.inventoryItemSummaryName}</strong>
                <span>x{item.inventoryItemSummaryAmount}</span>
              </div>
              {#if item.inventoryItemSummaryUsable}
                <button type="button" disabled={!state.connected || state.battle.active} on:click={() => sendAction({ use: item.inventoryItemSummaryId })}>
                  {translate(state.locale, "action.use")}
                </button>
              {/if}
            </article>
          {/each}
        </div>
      {/if}
    </section>
  {/if}
</aside>
