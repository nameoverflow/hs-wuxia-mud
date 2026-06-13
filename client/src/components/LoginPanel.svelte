<script lang="ts">
  import { connect, testEntryFromUrl, type GameState } from "../game";
  import { translate } from "../i18n";
  import { onMount } from "svelte";

  export let state: GameState;

  let username = "";
  let reset = false;

  onMount(() => {
    const entry = testEntryFromUrl();
    if (!entry) return;
    username = entry.user;
    reset = entry.reset;
    connect(entry.user, { reset: entry.reset });
  });

  function submit() {
    connect(username, { reset });
  }
</script>

<form class="login-panel" on:submit|preventDefault={submit}>
  <input
    type="text"
    bind:value={username}
    disabled={state.connected || state.connecting}
    placeholder={translate(state.locale, "field.name")}
    autocomplete="username"
  />
  <label class="reset-toggle">
    <input type="checkbox" bind:checked={reset} disabled={state.connected || state.connecting} />
    <span>{translate(state.locale, "ui.test_reset")}</span>
  </label>
  <button type="submit" class="primary-button" disabled={state.connected || state.connecting}>
    {translate(state.locale, "connection.connect")}
  </button>
  <p>{translate(state.locale, "ui.login_hint")}</p>
</form>
