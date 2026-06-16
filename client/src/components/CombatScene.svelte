<script lang="ts">
  import type { GameState } from "../game";
  import BattlePanel from "./BattlePanel.svelte";

  export let state: GameState;
</script>

<section class="world-panel combat-scene">
  <div class="world-grid">
    <BattlePanel state={state} />
  </div>
</section>
