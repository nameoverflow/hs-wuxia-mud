<script lang="ts">
  import { onMount } from "svelte";
  import { preloadBattleAssets } from "./battle/stageAssets";
  import { battleClock } from "./battle/battleClock";
  import CharacterPanel from "./components/CharacterPanel.svelte";
  import CombatScene from "./components/CombatScene.svelte";
  import LoginPanel from "./components/LoginPanel.svelte";
  import MartialArtsPanel from "./components/MartialArtsPanel.svelte";
  import MessageLog from "./components/MessageLog.svelte";
  import RightRail from "./components/RightRail.svelte";
  import RoomScene from "./components/RoomScene.svelte";
  import { game, setLocale, finishHiddenBattlePresentation } from "./game";
  import { translate } from "./i18n";

  onMount(() => {
    const load = () => { void preloadBattleAssets().catch(() => {}); };
    const idle = window as Window & { requestIdleCallback?: (callback: () => void, options?: { timeout: number }) => number };
    if (idle.requestIdleCallback) idle.requestIdleCallback(load, { timeout: 1500 });
    else window.setTimeout(load, 300);
    const visibility = () => {
      if (document.hidden) finishHiddenBattlePresentation();
      battleClock.suspend(document.hidden);
    };
    document.addEventListener("visibilitychange", visibility);
    visibility();
    return () => document.removeEventListener("visibilitychange", visibility);
  });
</script>

<svelte:head>
  <title>{translate($game.locale, "app.title")}</title>
</svelte:head>

<div class="app-shell" class:combat-mode={$game.battle.active}>
  <header class="app-header">
    <div class="brand">
      <span class="brand-mark">武</span>
      <div>
        <h1>{translate($game.locale, "app.title")}</h1>
        <p>WebSocket Jianghu Client</p>
      </div>
    </div>

    <div class="header-actions">
      <div class="locale-switch" aria-label="Language">
        <button type="button" class:active={$game.locale === "zh"} on:click={() => setLocale("zh")}>中</button>
        <button type="button" class:active={$game.locale === "en"} on:click={() => setLocale("en")}>EN</button>
      </div>
      <div class:online={$game.connected} class="connection-pill">
        <span></span>
        {$game.connected ? translate($game.locale, "connection.connected") : translate($game.locale, "connection.disconnected")}
      </div>
    </div>
  </header>

  <main class="main-layout" class:creation-layout={!$game.connected}>
    {#if !$game.connected}
      <div class="onboarding-column">
        <LoginPanel state={$game} />
      </div>
    {:else}
      <div class="left-column">
        <CharacterPanel state={$game} />
      </div>

      <div class="center-column">
        {#if $game.battle.active}
          <CombatScene state={$game} />
        {:else}
          <RoomScene state={$game} />
        {/if}
        <MessageLog state={$game} />
      </div>

      <div class="right-column">
        <RightRail state={$game} />
        {#if !$game.battle.active}
          <MartialArtsPanel state={$game} />
        {/if}
      </div>
    {/if}
  </main>
</div>
