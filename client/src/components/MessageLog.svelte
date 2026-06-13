<script lang="ts">
  import { afterUpdate } from "svelte";
  import { clearMessages, sendAction, type GameState } from "../game";
  import { translate } from "../i18n";

  export let state: GameState;

  let command = "";
  let log: HTMLDivElement;

  afterUpdate(() => {
    if (log) log.scrollTop = log.scrollHeight;
  });

  function submit() {
    const text = command.trim();
    if (!text) return;
    sendAction({ say: text });
    command = "";
  }
</script>

<section class="message-panel shell-panel">
  <div class="section-heading">
    <h2>{translate(state.locale, "panel.messages")}</h2>
    <button class="ghost-button" type="button" on:click={clearMessages}>{translate(state.locale, "action.clear")}</button>
  </div>

  <div bind:this={log} class="message-log">
    {#each state.messages as message}
      <article class={`log-line ${message.type}`}>
        <time>{message.time}</time>
        <p>{message.text}</p>
      </article>
    {/each}
  </div>

  <form class="command-row" on:submit|preventDefault={submit}>
    <input bind:value={command} disabled={!state.connected} placeholder={translate(state.locale, "ui.command_placeholder")} />
    <button type="submit" disabled={!state.connected}>{translate(state.locale, "action.send")}</button>
  </form>
</section>
