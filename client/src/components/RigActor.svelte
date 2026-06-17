<script lang="ts">
  import { onDestroy, onMount } from "svelte";
  import type { ActorVisual } from "../battle/animationTypes";
  import { createBattleActorRig, skeletalAnimationEntries } from "../battle/skeletal/catalog";
  import { interpolatePose, resolveRig } from "../battle/skeletal/runtime";
  import { SkeletalCanvasRenderer } from "../battle/skeletal/renderer";
  import type { SkeletalPoseDefinition, SkeletonRigDefinition } from "../battle/skeletal/types";

  export let visual: ActorVisual;
  export let durationMs = 720;

  let canvasEl: HTMLCanvasElement;
  let renderer: SkeletalCanvasRenderer | null = null;
  let ctx: CanvasRenderingContext2D | null = null;
  let frame = 0;
  let startedAt = 0;
  let lastRenderedAt = 0;

  $: entry = skeletalAnimationEntries.find((candidate) => candidate.id === visual.entryId) || skeletalAnimationEntries.find((candidate) => candidate.actionId === visual.actionId);
  $: rig = entry ? createBattleActorRig(entry) : null;
  $: sequence = visual.sequence?.length ? visual.sequence : [visual.poseId];
  $: renderKey = `${visual.entryId}:${visual.actionId}:${sequence.join(",")}:${durationMs}`;
  $: if (renderer && renderKey) restart();

  onMount(() => {
    renderer = new SkeletalCanvasRenderer(queueDraw);
    queueDraw();
    return stop;
  });

  onDestroy(stop);

  function queueDraw() {
    if (frame) cancelAnimationFrame(frame);
    frame = requestAnimationFrame(draw);
  }

  function stop() {
    if (frame) cancelAnimationFrame(frame);
    frame = 0;
  }

  function restart() {
    startedAt = 0;
    lastRenderedAt = 0;
    queueDraw();
  }

  function draw(now = performance.now()) {
    frame = 0;
    if (!startedAt) startedAt = now;
    if (!canvasEl || !renderer || !rig) return;
    const elapsed = now - startedAt;
    const animationMs = Math.max(120, durationMs);
    if (lastRenderedAt && now - lastRenderedAt < 1000 / 30 && elapsed < animationMs) {
      frame = requestAnimationFrame(draw);
      return;
    }
    lastRenderedAt = now;
    const rect = canvasEl.getBoundingClientRect();
    const dpr = Math.min(window.devicePixelRatio || 1, 2);
    const width = Math.max(256, Math.round(rect.width * dpr));
    const height = Math.max(192, Math.round(rect.height * dpr));
    if (canvasEl.width !== width || canvasEl.height !== height) {
      canvasEl.width = width;
      canvasEl.height = height;
    }
    ctx = ctx || canvasEl.getContext("2d");
    if (!ctx) return;
    const pose = poseAt(rig, now - startedAt, sequence, durationMs);
    const resolved = resolveRig(rig, pose, { bones: {}, bindings: {} });
    renderer.render(ctx, resolved, {
      showStage: false,
      showBones: false,
      showAnchors: false,
      showBindings: false,
      showImages: false,
      showSkin: true,
      showLabels: false,
      meshQuality: "full",
      zoom: 1.22,
      background: "transparent"
    });
    if (elapsed < animationMs) frame = requestAnimationFrame(draw);
  }

  function poseAt(rigValue: SkeletonRigDefinition, elapsedMs: number, poseIds: string[], totalMs: number): SkeletalPoseDefinition {
    const available = poseIds.filter((poseId) => rigValue.poses[poseId]);
    if (available.length === 0) return rigValue.poses.idle || Object.values(rigValue.poses)[0];
    if (available.length === 1) return rigValue.poses[available[0]];
    const segmentMs = Math.max(80, totalMs / (available.length - 1));
    const segmentIndex = Math.min(available.length - 2, Math.floor(elapsedMs / segmentMs));
    const from = rigValue.poses[available[segmentIndex]];
    const to = rigValue.poses[available[segmentIndex + 1]];
    const t = easeInOut((elapsedMs - segmentIndex * segmentMs) / segmentMs);
    return interpolatePose(from, to, t, `${visual.actionId}.${segmentIndex}`);
  }

  function easeInOut(value: number) {
    const t = Math.max(0, Math.min(1, value));
    return t * t * (3 - 2 * t);
  }
</script>

<canvas class="rig-actor-canvas" bind:this={canvasEl} aria-hidden="true"></canvas>
