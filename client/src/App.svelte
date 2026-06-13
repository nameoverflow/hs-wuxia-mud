<script lang="ts">
  import BattlePanel from "./components/BattlePanel.svelte";
  import CharacterPanel from "./components/CharacterPanel.svelte";
  import LoginPanel from "./components/LoginPanel.svelte";
  import MartialArtsPanel from "./components/MartialArtsPanel.svelte";
  import MessageLog from "./components/MessageLog.svelte";
  import RightRail from "./components/RightRail.svelte";
  import RoomScene from "./components/RoomScene.svelte";
  import { game, setLocale } from "./game";
  import { translate } from "./i18n";
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

  <main class="main-layout">
    <div class="left-column">
      <CharacterPanel state={$game} />
      <LoginPanel state={$game} />
    </div>

    <div class="center-column">
      <BattlePanel state={$game} />
      <RoomScene state={$game} />
      <MessageLog state={$game} />
    </div>

    <div class="right-column">
      <RightRail state={$game} />
      <MartialArtsPanel state={$game} />
    </div>
  </main>
</div>
