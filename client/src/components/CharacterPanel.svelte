<script lang="ts">
  import { formatStatus, sendAction, type GameState } from "../game";
  import { translate } from "../i18n";
  import portrait00 from "../assets/portraits/beauty-score-00.png";
  import portrait02 from "../assets/portraits/beauty-score-02.png";
  import portrait04 from "../assets/portraits/beauty-score-04.png";
  import portrait06 from "../assets/portraits/beauty-score-06.png";
  import portrait08 from "../assets/portraits/beauty-score-08.png";
  import portrait10 from "../assets/portraits/beauty-score-10.png";
  import ResourceMeter from "./ResourceMeter.svelte";

  export let state: GameState;

  let activeTab: "status" | "inventory" = "status";
  $: presentedHp = state.battle.active ? state.battle.presentation?.playerHp ?? state.stats.hp : state.stats.hp;

  const requestRefresh = () => {
    sendAction({ other: "view" });
    sendAction({ other: "quests" });
    sendAction({ other: "inventory" });
    sendAction({ other: "arts" });
  };

  const genderLabel = (gender: string) => translate(state.locale, `gender.${gender || "unknown"}`);
  const portraitByScore: Record<string, string> = {
    "00": portrait00,
    "02": portrait02,
    "04": portrait04,
    "06": portrait06,
    "08": portrait08,
    "10": portrait10
  };

  const portraitScore = (appearance: number) => Math.max(0, Math.min(10, Math.round(appearance / 2) * 2));
  const portraitSrc = (gender: string, appearance: number) => {
    const score = String(portraitScore(appearance)).padStart(2, "0");
    return portraitByScore[`${gender || "unknown"}-${score}`] || portraitByScore[score] || portrait04;
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
    <figure class="portrait-frame">
      <img src={portraitSrc(state.stats.gender, state.stats.appearance)} alt={`${state.username || translate(state.locale, "panel.character")} ${translate(state.locale, "field.appearance")}`} />
      <figcaption>
        <strong>{state.username || "-"}</strong>
        <span>{translate(state.locale, "field.appearance")} {state.stats.appearance}</span>
      </figcaption>
    </figure>

    <dl class="identity-grid">
      <div>
        <dt>{translate(state.locale, "field.name")}</dt>
        <dd>{state.username || "-"}</dd>
      </div>
      <div>
        <dt>{translate(state.locale, "field.gender")}</dt>
        <dd>{genderLabel(state.stats.gender)}</dd>
      </div>
      <div>
        <dt>{translate(state.locale, "field.appearance")}</dt>
        <dd>{state.stats.appearance} / {state.stats.appearanceText || "-"}</dd>
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
      <ResourceMeter label={translate(state.locale, "resource.hp")} value={presentedHp} max={state.stats.maxHp} tone="hp" snapDecrease={state.battle.active} />
      <ResourceMeter label={translate(state.locale, "resource.qi")} value={state.stats.qi} max={state.stats.maxQi} tone="qi" />
      <ResourceMeter label={translate(state.locale, "resource.jing")} value={state.stats.jing} max={state.stats.maxJing} tone="ap" />
    </div>

    <section class="compact-section">
      <h3>{translate(state.locale, "panel.innate")}</h3>
      <dl class="attribute-grid">
        <div>
          <dt>{translate(state.locale, "attr.strength")}</dt>
          <dd>{state.stats.strength}</dd>
        </div>
        <div>
          <dt>{translate(state.locale, "attr.agility")}</dt>
          <dd>{state.stats.agility}</dd>
        </div>
        <div>
          <dt>{translate(state.locale, "attr.vitality")}</dt>
          <dd>{state.stats.vitality}</dd>
        </div>
      </dl>
    </section>

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
                <button type="button" disabled={!state.connected || state.battle.active || state.storyActive} on:click={() => sendAction({ use: item.inventoryItemSummaryId })}>
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
