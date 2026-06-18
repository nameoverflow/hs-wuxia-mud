<script lang="ts">
  import { percent } from "../game";
  import { meterTween } from "./meterTween";

  export let label: string;
  export let value: number;
  export let max: number;
  export let tone: "hp" | "qi" | "ap" | "xp" = "hp";
</script>

<div class="meter meter-{tone}">
  <div class="meter-row">
    <span>{label}</span>
    <strong>{value}/{max}</strong>
  </div>
  <div class="meter-track" aria-label={label}>
    <span use:meterTween={{ value: percent(value, max) / 100 }}></span>
  </div>
</div>
