<script lang="ts">
  import { onMount } from "svelte";
  import { createBattleActorRig, skeletalAnimationEntries } from "../../battle/skeletal/catalog";
  import { angleBetween, applyToPoint, distance, invert, normalizeDegrees } from "../../battle/skeletal/math";
  import { interpolatePose, resolveRig } from "../../battle/skeletal/runtime";
  import { fitRigViewport, screenPointToRig, SkeletalCanvasRenderer } from "../../battle/skeletal/renderer";
  import type {
    AnchorPose,
    AnimationRigEntry,
    BindingPose,
    BonePose,
    PoseOverrides,
    ResolvedRig,
    RigViewport,
    SkeletalPoseDefinition,
    SkeletonRigDefinition,
    Vec2
  } from "../../battle/skeletal/types";
  import type { CombatStyle, VisualProfile } from "../../battle/animationTypes";

  type ProfileFilter = VisualProfile | "all";
  type StyleFilter = CombatStyle | "all";
  type DragMode = "pose" | "anchor" | "binding";
  type DragState =
    | { mode: "pose"; boneId: string }
    | { mode: "poseAnchor"; anchorId: string }
    | { mode: "anchor"; anchorId: string }
    | { mode: "binding"; bindingId: string };

  const entries = skeletalAnimationEntries;

  let canvasEl: HTMLCanvasElement;
  let renderer: SkeletalCanvasRenderer | null = null;
  let viewport: RigViewport | null = null;
  let resizeObserver: ResizeObserver | null = null;
  let drawFrame = 0;

  let selectedEntryId = entries[0]?.id || "";
  let selectedPoseId = entries[0]?.poseId || "idle";
  let lastEntryId = "";
  let profileFilter: ProfileFilter = "all";
  let styleFilter: StyleFilter = "all";
  let search = "";
  let dragMode: DragMode = "pose";
  let dragState: DragState | null = null;
  let selectedBoneId = "frontArm";
  let selectedAnchorId = entries[0]?.tags.includes("part-rig") ? "frontWrist" : "frontHand";
  let selectedBindingId = entries[0]?.tags.includes("part-rig") ? "part.arm_front" : "source.frame";
  let isPlaying = false;
  let playbackTime = 0;
  let playbackLastMs = 0;
  let showBones = true;
  let showAnchors = true;
  let showBindings = true;
  let showImages = true;
  let showSkin = true;
  let showLabels = true;
  let lockLimbLengths = true;
  let zoom = 2.2;
  let viewportPan: Vec2 = { x: 0, y: 0 };
  let boneOverridesByPose: Record<string, Record<string, BonePose>> = {};
  let anchorOverridesByPose: Record<string, Record<string, AnchorPose>> = {};
  let anchorOverrides: Record<string, AnchorPose> = {};
  let bindingOverrides: Record<string, BindingPose> = {};
  let importText = "";
  let saveStatus = "";

  $: filteredEntries = filterEntries(entries, profileFilter, styleFilter, search);
  $: selectedEntry = entries.find((entry) => entry.id === selectedEntryId) || entries[0];
  $: rig = selectedEntry ? createBattleActorRig(selectedEntry) : null;
  $: if (selectedEntry && selectedEntry.id !== lastEntryId) {
    lastEntryId = selectedEntry.id;
    selectedPoseId = selectedEntry.poseId;
    isPlaying = false;
    playbackTime = 0;
    playbackLastMs = 0;
    selectedBoneId = selectedEntry.tags.includes("part-rig") ? "frontArm" : "frontForearm";
    selectedAnchorId = selectedEntry.tags.includes("part-rig") ? "frontWrist" : "frontHand";
    selectedBindingId = selectedEntry.tags.includes("part-rig") ? "part.arm_front" : "source.frame";
    loadSavedEdits(selectedEntry.id);
  }
  $: selectedPose = rig?.poses[selectedPoseId] || (selectedEntry && rig?.poses[selectedEntry.poseId]) || rig?.poses.idle;
  $: activePose = rig && isPlaying ? playbackPose(rig, playbackTime, selectedPoseId) : selectedPose;
  $: currentBoneOverrides = isPlaying ? {} : boneOverridesByPose[selectedPoseId] || {};
  $: currentPoseAnchorOverrides = isPlaying ? {} : anchorOverridesByPose[selectedPoseId] || {};
  $: currentAnchorOverrides = { ...anchorOverrides, ...currentPoseAnchorOverrides };
  $: overrides = { bones: currentBoneOverrides, anchors: currentAnchorOverrides, bindings: bindingOverrides };
  $: resolvedRig = rig && activePose ? resolveRig(rig, activePose, overrides) : null;
  $: if (resolvedRig && !resolvedRig.bonesById[selectedBoneId]) selectedBoneId = resolvedRig.bones[0]?.id || "";
  $: if (resolvedRig && !resolvedRig.anchorsById[selectedAnchorId]) selectedAnchorId = resolvedRig.anchors[0]?.id || "";
  $: if (resolvedRig && !resolvedRig.bindingsById[selectedBindingId]) selectedBindingId = resolvedRig.bindings[0]?.id || "";
  $: selectedBone = resolvedRig?.bonesById[selectedBoneId] || null;
  $: selectedAnchor = resolvedRig?.anchorsById[selectedAnchorId] || null;
  $: selectedBinding = resolvedRig?.bindingsById[selectedBindingId] || null;
  $: exportText = makeExportText(selectedEntry, selectedPoseId, overrides);
  $: canSaveProjectPoses = !!selectedEntry?.tags.includes("part-rig");
  $: renderInvalidationKey = [
    showBones,
    showAnchors,
    showBindings,
    showImages,
    showSkin,
    showLabels,
    selectedBoneId,
    selectedAnchorId,
    selectedBindingId,
    zoom,
    viewportPan.x,
    viewportPan.y
  ].join(":");
  $: if (resolvedRig && canvasEl && !isPlaying && renderInvalidationKey) queueDraw();

  onMount(() => {
    renderer = new SkeletalCanvasRenderer(queueDraw);
    resizeObserver = new ResizeObserver(queueDraw);
    if (canvasEl) resizeObserver.observe(canvasEl);
    queueDraw();
    return () => {
      resizeObserver?.disconnect();
      if (drawFrame) cancelAnimationFrame(drawFrame);
    };
  });

  function filterEntries(list: AnimationRigEntry[], profile: ProfileFilter, style: StyleFilter, term: string) {
    const q = term.trim().toLowerCase();
    return list.filter((entry) => {
      if (profile !== "all" && entry.profile !== profile) return false;
      if (style !== "all" && entry.style !== style) return false;
      if (!q) return true;
      return [entry.actionId, entry.clipId, entry.profile, entry.style, ...entry.tags].join(" ").toLowerCase().includes(q);
    });
  }

  function selectEntry(id: string) {
    selectedEntryId = id;
  }

  function selectPose(id: string) {
    selectedPoseId = id;
    isPlaying = false;
    playbackTime = 0;
    playbackLastMs = 0;
    queueDraw();
  }

  function queueDraw() {
    if (drawFrame) cancelAnimationFrame(drawFrame);
    drawFrame = requestAnimationFrame(drawCanvas);
  }

  function drawCanvas(now = performance.now()) {
    drawFrame = 0;
    if (isPlaying) {
      if (!playbackLastMs) playbackLastMs = now;
      playbackTime += Math.min(64, now - playbackLastMs);
      playbackLastMs = now;
    } else {
      playbackLastMs = 0;
    }
    if (!canvasEl || !renderer || !resolvedRig) return;
    const rect = canvasEl.getBoundingClientRect();
    const dpr = window.devicePixelRatio || 1;
    const width = Math.max(420, Math.round(rect.width * dpr));
    const height = Math.max(360, Math.round(rect.height * dpr));
    if (canvasEl.width !== width || canvasEl.height !== height) {
      canvasEl.width = width;
      canvasEl.height = height;
    }
    const ctx = canvasEl.getContext("2d");
    if (!ctx) return;
    viewport = renderer.render(ctx, resolvedRig, {
      showBones,
      showAnchors,
      showBindings,
      showImages,
      showSkin,
      showLabels,
      selectedBoneId,
      selectedAnchorId,
      selectedBindingId,
      zoom,
      panX: viewportPan.x,
      panY: viewportPan.y
    });
    if (isPlaying) drawFrame = requestAnimationFrame(drawCanvas);
  }

  function togglePlayback() {
    isPlaying = !isPlaying;
    playbackLastMs = 0;
    if (isPlaying) queueDraw();
  }

  function resetPlayback() {
    playbackTime = 0;
    playbackLastMs = 0;
    queueDraw();
  }

  function stepPose(direction: -1 | 1) {
    if (!rig) return;
    const ids = keyframePoseIds(rig);
    if (ids.length === 0) return;
    const currentIndex = Math.max(0, ids.indexOf(selectedPoseId));
    const nextIndex = (currentIndex + direction + ids.length) % ids.length;
    selectPose(ids[nextIndex]);
  }

  function handleCanvasWheel(event: WheelEvent) {
    if (!canvasEl || !rig) return;
    event.preventDefault();
    const rect = canvasEl.getBoundingClientRect();
    const dpr = window.devicePixelRatio || 1;
    const point = {
      x: (event.clientX - rect.left) * dpr,
      y: (event.clientY - rect.top) * dpr
    };

    if (event.ctrlKey || event.metaKey) {
      const delta = normalizedWheelDelta(event, rect);
      setZoomAroundPoint(zoom * Math.exp(-delta * 0.0018), point);
      return;
    }

    const unit = wheelUnit(event, rect);
    const deltaX = event.shiftKey ? event.deltaY : event.deltaX;
    const deltaY = event.shiftKey ? 0 : event.deltaY;
    viewportPan = {
      x: round(viewportPan.x - deltaX * unit * dpr),
      y: round(viewportPan.y - deltaY * unit * dpr)
    };
    queueDraw();
  }

  function handleZoomInput(event: Event) {
    if (!canvasEl) {
      zoom = clampZoom(numericInput(event));
      return;
    }
    const rect = canvasEl.getBoundingClientRect();
    const dpr = window.devicePixelRatio || 1;
    setZoomAroundPoint(clampZoom(numericInput(event)), {
      x: (rect.width * dpr) / 2,
      y: (rect.height * dpr) / 2
    });
  }

  function resetView() {
    zoom = 2.2;
    viewportPan = { x: 0, y: 0 };
    queueDraw();
  }

  function setZoomAroundPoint(nextZoom: number, point: Vec2) {
    if (!canvasEl || !rig) return;
    const next = clampZoom(nextZoom);
    const currentViewport = viewport || {
      ...fitRigViewport(canvasEl.width, canvasEl.height, rig.canvas, zoom),
      offsetX: fitRigViewport(canvasEl.width, canvasEl.height, rig.canvas, zoom).offsetX + viewportPan.x,
      offsetY: fitRigViewport(canvasEl.width, canvasEl.height, rig.canvas, zoom).offsetY + viewportPan.y
    };
    const rigPoint = screenPointToRig(point, currentViewport);
    const nextBase = fitRigViewport(canvasEl.width, canvasEl.height, rig.canvas, next);
    zoom = next;
    viewportPan = {
      x: round(point.x - nextBase.offsetX - rigPoint.x * nextBase.scale),
      y: round(point.y - nextBase.offsetY - rigPoint.y * nextBase.scale)
    };
    queueDraw();
  }

  function normalizedWheelDelta(event: WheelEvent, rect: DOMRect) {
    return event.deltaY * wheelUnit(event, rect);
  }

  function wheelUnit(event: WheelEvent, rect: DOMRect) {
    if (event.deltaMode === WheelEvent.DOM_DELTA_LINE) return 18;
    if (event.deltaMode === WheelEvent.DOM_DELTA_PAGE) return Math.max(rect.width, rect.height);
    return 1;
  }

  function clampZoom(value: number) {
    return Math.max(0.45, Math.min(7, value));
  }

  function pointerPoint(event: PointerEvent): Vec2 | null {
    if (!canvasEl || !viewport) return null;
    const rect = canvasEl.getBoundingClientRect();
    const dpr = window.devicePixelRatio || 1;
    return screenPointToRig({ x: (event.clientX - rect.left) * dpr, y: (event.clientY - rect.top) * dpr }, viewport);
  }

  function handlePointerDown(event: PointerEvent) {
    if (!resolvedRig || !canvasEl) return;
    const point = pointerPoint(event);
    if (!point) return;
    canvasEl.setPointerCapture(event.pointerId);

    if (dragMode === "anchor") {
      const anchor = nearestAnchor(resolvedRig, point);
      if (anchor) {
        selectedAnchorId = anchor.id;
        if (anchor.definition.handleBoneId) selectedBoneId = anchor.definition.handleBoneId;
        dragState = { mode: "anchor", anchorId: anchor.id };
        dragAnchorTo(anchor.id, point);
      }
      return;
    }

    if (dragMode === "binding") {
      const binding = nearestBinding(resolvedRig, point);
      if (binding) {
        selectedBindingId = binding.id;
        dragState = { mode: "binding", bindingId: binding.id };
        dragBindingTo(binding.id, point);
        return;
      }
    }

    const anchor = nearestAnchor(resolvedRig, point);
    if (anchor) {
      selectedAnchorId = anchor.id;
      if (isDeformKeypoint(anchor.id)) {
        dragState = { mode: "poseAnchor", anchorId: anchor.id };
        dragPoseAnchorTo(anchor.id, point);
        return;
      }
      if (anchor.definition.handleBoneId) {
        selectedBoneId = anchor.definition.handleBoneId;
        dragState = { mode: "pose", boneId: anchor.definition.handleBoneId };
        rotateBoneTo(anchor.definition.handleBoneId, point);
      }
      return;
    }

    const binding = nearestBinding(resolvedRig, point);
    if (binding) selectedBindingId = binding.id;
  }

  function handlePointerMove(event: PointerEvent) {
    if (!dragState) return;
    const point = pointerPoint(event);
    if (!point) return;
    if (dragState.mode === "pose") rotateBoneTo(dragState.boneId, point);
    else if (dragState.mode === "poseAnchor") dragPoseAnchorTo(dragState.anchorId, point);
    else if (dragState.mode === "anchor") dragAnchorTo(dragState.anchorId, point);
    else dragBindingTo(dragState.bindingId, point);
  }

  function handlePointerUp(event: PointerEvent) {
    canvasEl?.releasePointerCapture(event.pointerId);
    dragState = null;
  }

  function rotateBoneTo(boneId: string, point: Vec2) {
    if (!resolvedRig) return;
    const bone = resolvedRig.bonesById[boneId];
    if (!bone) return;
    const parentRotation = bone.parentId ? resolvedRig.bonesById[bone.parentId]?.worldRotation || 0 : 0;
    const localRotation = normalizeDegrees(angleBetween(bone.start, point) - parentRotation);
    setBoneOverride(boneId, { rotation: round(localRotation) });
  }

  function dragBindingTo(bindingId: string, point: Vec2) {
    if (!resolvedRig) return;
    const binding = resolvedRig.bindingsById[bindingId];
    if (!binding) return;
    const local = applyToPoint(invert(binding.anchor.matrix), point);
    setBindingOverride(bindingId, { offsetX: round(local.x), offsetY: round(local.y) });
  }

  function dragAnchorTo(anchorId: string, point: Vec2) {
    if (!resolvedRig) return;
    const anchor = resolvedRig.anchorsById[anchorId];
    if (!anchor) return;
    const bone = resolvedRig.bonesById[anchor.definition.boneId];
    const local = bone ? applyToPoint(invert(bone.matrix), point) : point;
    setAnchorOverride(anchorId, { x: round(local.x), y: round(local.y) });
  }

  function dragPoseAnchorTo(anchorId: string, point: Vec2) {
    if (!resolvedRig) return;
    if (lockLimbLengths && applyConstrainedPoseAnchorDrag(anchorId, point)) return;
    const anchor = resolvedRig.anchorsById[anchorId];
    if (!anchor) return;
    const bone = resolvedRig.bonesById[anchor.definition.boneId];
    const local = bone ? applyToPoint(invert(bone.matrix), point) : point;
    setPoseAnchorOverride(anchorId, { x: round(local.x), y: round(local.y) });
  }

  function applyConstrainedPoseAnchorDrag(anchorId: string, point: Vec2) {
    if (!resolvedRig) return false;
    const chain = deformChainForAnchor(anchorId);
    if (!chain) return false;
    const anchors = chain.anchorIds.map((id) => resolvedRig?.anchorsById[id]).filter((anchor) => !!anchor);
    const points = anchors.map((anchor) => anchor.position);
    if (anchors.length === 5) return applyConstrainedFivePointDrag(chain.anchorIds, chain.index, points, point);
    if (anchors.length === 4) return applyConstrainedFourPointDrag(chain.anchorIds, chain.index, points, point);
    if (anchors.length !== 3) return false;
    const upperLength = Math.max(0.1, distance(points[0], points[1]));
    const lowerLength = Math.max(0.1, distance(points[1], points[2]));

    if (chain.index === 0) {
      const delta = { x: point.x - points[0].x, y: point.y - points[0].y };
      setPoseAnchorWorldOverrides({
        [chain.anchorIds[0]]: point,
        [chain.anchorIds[1]]: { x: points[1].x + delta.x, y: points[1].y + delta.y },
        [chain.anchorIds[2]]: { x: points[2].x + delta.x, y: points[2].y + delta.y }
      });
      return true;
    }

    if (chain.index === 1) {
      setPoseAnchorWorldOverrides({
        [chain.anchorIds[1]]: solveConstrainedMidpoint(points[0], points[2], upperLength, lowerLength, point)
      });
      return true;
    }

    const solution = solveTwoBoneIk(points[0], point, upperLength, lowerLength, points[1]);
    setPoseAnchorWorldOverrides({
      [chain.anchorIds[1]]: solution.mid,
      [chain.anchorIds[2]]: solution.end
    });
    return true;
  }

  function applyConstrainedFivePointDrag(anchorIds: string[], index: number, points: Vec2[], point: Vec2) {
    const rootLength = Math.max(0.1, distance(points[0], points[1]));
    const upperLength = Math.max(0.1, distance(points[1], points[2]));
    const lowerLength = Math.max(0.1, distance(points[2], points[3]));
    const terminalLength = Math.max(0.1, distance(points[3], points[4]));

    if (index === 0) {
      const delta = { x: point.x - points[0].x, y: point.y - points[0].y };
      setPoseAnchorWorldOverrides({
        [anchorIds[0]]: point,
        [anchorIds[1]]: { x: points[1].x + delta.x, y: points[1].y + delta.y },
        [anchorIds[2]]: { x: points[2].x + delta.x, y: points[2].y + delta.y },
        [anchorIds[3]]: { x: points[3].x + delta.x, y: points[3].y + delta.y },
        [anchorIds[4]]: { x: points[4].x + delta.x, y: points[4].y + delta.y }
      });
      return true;
    }

    if (index === 1) {
      const direction = directionBetween(points[0], point, directionBetween(points[0], points[1], { x: 1, y: 0 }));
      const shoulder = {
        x: points[0].x + direction.x * rootLength,
        y: points[0].y + direction.y * rootLength
      };
      const delta = { x: shoulder.x - points[1].x, y: shoulder.y - points[1].y };
      setPoseAnchorWorldOverrides({
        [anchorIds[1]]: shoulder,
        [anchorIds[2]]: { x: points[2].x + delta.x, y: points[2].y + delta.y },
        [anchorIds[3]]: { x: points[3].x + delta.x, y: points[3].y + delta.y },
        [anchorIds[4]]: { x: points[4].x + delta.x, y: points[4].y + delta.y }
      });
      return true;
    }

    if (index === 2) {
      setPoseAnchorWorldOverrides({
        [anchorIds[2]]: solveConstrainedMidpoint(points[1], points[3], upperLength, lowerLength, point)
      });
      return true;
    }

    if (index === 3) {
      const solution = solveTwoBoneIk(points[1], point, upperLength, lowerLength, points[2]);
      const delta = { x: solution.end.x - points[3].x, y: solution.end.y - points[3].y };
      setPoseAnchorWorldOverrides({
        [anchorIds[2]]: solution.mid,
        [anchorIds[3]]: solution.end,
        [anchorIds[4]]: { x: points[4].x + delta.x, y: points[4].y + delta.y }
      });
      return true;
    }

    const terminalDirection = directionBetween(points[3], point, directionBetween(points[3], points[4], directionBetween(points[2], points[3], { x: 1, y: 0 })));
    setPoseAnchorWorldOverrides({
      [anchorIds[4]]: {
        x: points[3].x + terminalDirection.x * terminalLength,
        y: points[3].y + terminalDirection.y * terminalLength
      }
    });
    return true;
  }

  function applyConstrainedFourPointDrag(anchorIds: string[], index: number, points: Vec2[], point: Vec2) {
    const rootLength = Math.max(0.1, distance(points[0], points[1]));
    const upperLength = Math.max(0.1, distance(points[1], points[2]));
    const lowerLength = Math.max(0.1, distance(points[2], points[3]));

    if (index === 0) {
      const delta = { x: point.x - points[0].x, y: point.y - points[0].y };
      setPoseAnchorWorldOverrides({
        [anchorIds[0]]: point,
        [anchorIds[1]]: { x: points[1].x + delta.x, y: points[1].y + delta.y },
        [anchorIds[2]]: { x: points[2].x + delta.x, y: points[2].y + delta.y },
        [anchorIds[3]]: { x: points[3].x + delta.x, y: points[3].y + delta.y }
      });
      return true;
    }

    if (index === 1) {
      const direction = directionBetween(points[0], point, directionBetween(points[0], points[1], { x: 1, y: 0 }));
      const shoulder = {
        x: points[0].x + direction.x * rootLength,
        y: points[0].y + direction.y * rootLength
      };
      const delta = { x: shoulder.x - points[1].x, y: shoulder.y - points[1].y };
      setPoseAnchorWorldOverrides({
        [anchorIds[1]]: shoulder,
        [anchorIds[2]]: { x: points[2].x + delta.x, y: points[2].y + delta.y },
        [anchorIds[3]]: { x: points[3].x + delta.x, y: points[3].y + delta.y }
      });
      return true;
    }

    if (index === 2) {
      setPoseAnchorWorldOverrides({
        [anchorIds[2]]: solveConstrainedMidpoint(points[1], points[3], upperLength, lowerLength, point)
      });
      return true;
    }

    const solution = solveTwoBoneIk(points[1], point, upperLength, lowerLength, points[2]);
    setPoseAnchorWorldOverrides({
      [anchorIds[2]]: solution.mid,
      [anchorIds[3]]: solution.end
    });
    return true;
  }

  function deformChainForAnchor(anchorId: string) {
    if (!rig) return null;
    for (const binding of rig.bindings) {
      const anchorIds = binding.deform?.keypoints.map((keypoint) => keypoint.anchorId);
      const index = anchorIds?.indexOf(anchorId) ?? -1;
      if (anchorIds && index >= 0) return { anchorIds, index };
    }
    return null;
  }

  function setPoseAnchorWorldOverrides(pointsByAnchorId: Record<string, Vec2>) {
    if (!resolvedRig) return;
    isPlaying = false;
    const poseOverrides = anchorOverridesByPose[selectedPoseId] || {};
    const next = { ...poseOverrides };
    for (const [anchorId, worldPoint] of Object.entries(pointsByAnchorId)) {
      const anchor = resolvedRig.anchorsById[anchorId];
      if (!anchor) continue;
      const bone = resolvedRig.bonesById[anchor.definition.boneId];
      const local = bone ? applyToPoint(invert(bone.matrix), worldPoint) : worldPoint;
      next[anchorId] = { ...(next[anchorId] || {}), x: round(local.x), y: round(local.y) };
    }
    anchorOverridesByPose = { ...anchorOverridesByPose, [selectedPoseId]: next };
  }

  function solveTwoBoneIk(root: Vec2, target: Vec2, upperLength: number, lowerLength: number, currentMid: Vec2): { mid: Vec2; end: Vec2 } {
    const direction = directionBetween(root, target, directionBetween(root, currentMid, { x: 1, y: 0 }));
    const desiredDistance = distance(root, target);
    const minReach = Math.max(0.001, Math.abs(upperLength - lowerLength) + 0.001);
    const maxReach = Math.max(minReach, upperLength + lowerLength - 0.001);
    const solvedDistance = Math.max(minReach, Math.min(maxReach, desiredDistance));
    const end = {
      x: root.x + direction.x * solvedDistance,
      y: root.y + direction.y * solvedDistance
    };
    return {
      end,
      mid: solveConstrainedMidpoint(root, end, upperLength, lowerLength, currentMid)
    };
  }

  function solveConstrainedMidpoint(root: Vec2, end: Vec2, upperLength: number, lowerLength: number, preferredMid: Vec2): Vec2 {
    const d = distance(root, end);
    if (d < 0.001 || d > upperLength + lowerLength || d < Math.abs(upperLength - lowerLength)) {
      const fallbackDirection = directionBetween(root, preferredMid, { x: 1, y: 0 });
      return {
        x: root.x + fallbackDirection.x * upperLength,
        y: root.y + fallbackDirection.y * upperLength
      };
    }
    const direction = directionBetween(root, end, { x: 1, y: 0 });
    const along = (upperLength * upperLength - lowerLength * lowerLength + d * d) / (2 * d);
    const height = Math.sqrt(Math.max(0, upperLength * upperLength - along * along));
    const base = { x: root.x + direction.x * along, y: root.y + direction.y * along };
    const normal = { x: -direction.y, y: direction.x };
    const a = { x: base.x + normal.x * height, y: base.y + normal.y * height };
    const b = { x: base.x - normal.x * height, y: base.y - normal.y * height };
    return distance(a, preferredMid) <= distance(b, preferredMid) ? a : b;
  }

  function directionBetween(from: Vec2, to: Vec2, fallback: Vec2): Vec2 {
    const dx = to.x - from.x;
    const dy = to.y - from.y;
    const length = Math.hypot(dx, dy);
    if (length < 0.001) return fallback;
    return { x: dx / length, y: dy / length };
  }

  function nearestAnchor(rigValue: ResolvedRig, point: Vec2) {
    const maxDistance = 9 / (viewport?.scale || 1);
    return rigValue.anchors
      .map((anchor) => ({ anchor, distance: distance(anchor.position, point) }))
      .filter((hit) => hit.distance <= maxDistance)
      .sort((a, b) => a.distance - b.distance)[0]?.anchor;
  }

  function nearestBinding(rigValue: ResolvedRig, point: Vec2) {
    const maxDistance = 12 / (viewport?.scale || 1);
    return rigValue.bindings
      .map((binding) => ({ binding, distance: distance(binding.position, point) }))
      .filter((hit) => hit.distance <= maxDistance)
      .sort((a, b) => a.distance - b.distance)[0]?.binding;
  }

  function setBoneOverride(boneId: string, patch: BonePose) {
    isPlaying = false;
    const poseOverrides = boneOverridesByPose[selectedPoseId] || {};
    boneOverridesByPose = {
      ...boneOverridesByPose,
      [selectedPoseId]: {
        ...poseOverrides,
        [boneId]: { ...(poseOverrides[boneId] || {}), ...patch }
      }
    };
  }

  function setBindingOverride(bindingId: string, patch: BindingPose) {
    bindingOverrides = {
      ...bindingOverrides,
      [bindingId]: { ...(bindingOverrides[bindingId] || {}), ...patch }
    };
  }

  function setAnchorOverride(anchorId: string, patch: AnchorPose) {
    anchorOverrides = {
      ...anchorOverrides,
      [anchorId]: { ...(anchorOverrides[anchorId] || {}), ...patch }
    };
  }

  function setPoseAnchorOverride(anchorId: string, patch: AnchorPose) {
    isPlaying = false;
    const poseOverrides = anchorOverridesByPose[selectedPoseId] || {};
    anchorOverridesByPose = {
      ...anchorOverridesByPose,
      [selectedPoseId]: {
        ...poseOverrides,
        [anchorId]: { ...(poseOverrides[anchorId] || {}), ...patch }
      }
    };
  }

  function updateSelectedBone(patch: BonePose) {
    if (!selectedBoneId) return;
    setBoneOverride(selectedBoneId, patch);
  }

  function updateSelectedBinding(patch: BindingPose) {
    if (!selectedBindingId) return;
    setBindingOverride(selectedBindingId, patch);
  }

  function updateSelectedAnchor(patch: AnchorPose) {
    if (!selectedAnchorId) return;
    setAnchorOverride(selectedAnchorId, patch);
  }

  function updateSelectedPoseAnchor(patch: AnchorPose) {
    if (!selectedAnchorId) return;
    if (lockLimbLengths && resolvedRig && isDeformKeypoint(selectedAnchorId)) {
      const anchor = resolvedRig.anchorsById[selectedAnchorId];
      const bone = anchor ? resolvedRig.bonesById[anchor.definition.boneId] : null;
      if (anchor && bone) {
        const targetLocal = {
          x: patch.x ?? anchor.definition.x,
          y: patch.y ?? anchor.definition.y
        };
        const targetWorld = applyToPoint(bone.matrix, targetLocal);
        if (applyConstrainedPoseAnchorDrag(selectedAnchorId, targetWorld)) return;
      }
    }
    setPoseAnchorOverride(selectedAnchorId, patch);
  }

  function resetSelectedBone() {
    const poseOverrides = boneOverridesByPose[selectedPoseId] || {};
    const { [selectedBoneId]: _, ...rest } = poseOverrides;
    boneOverridesByPose = { ...boneOverridesByPose, [selectedPoseId]: rest };
  }

  function resetSelectedBinding() {
    const { [selectedBindingId]: _, ...rest } = bindingOverrides;
    bindingOverrides = rest;
  }

  function resetSelectedAnchor() {
    const { [selectedAnchorId]: _, ...rest } = anchorOverrides;
    anchorOverrides = rest;
  }

  function resetSelectedPoseAnchor() {
    const poseOverrides = anchorOverridesByPose[selectedPoseId] || {};
    const { [selectedAnchorId]: _, ...rest } = poseOverrides;
    anchorOverridesByPose = { ...anchorOverridesByPose, [selectedPoseId]: rest };
  }

  function resetAll() {
    boneOverridesByPose = {};
    anchorOverridesByPose = {};
    anchorOverrides = {};
    bindingOverrides = {};
  }

  function saveEdits() {
    if (!selectedEntry) return;
    localStorage.setItem(storageKey(selectedEntry.id), exportText);
    saveStatus = "Saved";
  }

  async function saveProjectPoses() {
    if (!selectedEntry || !rig || !canSaveProjectPoses) {
      saveStatus = "Project save unavailable";
      return;
    }

    try {
      const response = await fetch("/__rig/segmented-v12-poses", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ poses: mergedProjectPoseLibrary(rig) })
      });
      if (!response.ok) throw new Error(await response.text());
      localStorage.removeItem(storageKey(selectedEntry.id));
      resetAll();
      saveStatus = "Saved Project";
      window.setTimeout(() => window.location.reload(), 240);
    } catch (error) {
      saveStatus = error instanceof Error ? `Save failed: ${error.message}` : "Save failed";
    }
  }

  function clearSavedEdits() {
    if (!selectedEntry) return;
    localStorage.removeItem(storageKey(selectedEntry.id));
    resetAll();
    saveStatus = "Cleared";
  }

  function loadSavedEdits(entryId: string) {
    const raw = localStorage.getItem(storageKey(entryId));
    if (!raw) {
      boneOverridesByPose = {};
      anchorOverridesByPose = {};
      anchorOverrides = {};
      bindingOverrides = {};
      importText = "";
      saveStatus = "";
      return;
    }
    applyImport(raw, false);
    saveStatus = "Loaded";
  }

  function applyImport(raw = importText, updateStatus = true) {
    try {
      const parsed = JSON.parse(raw) as {
        poseId?: string;
        bones?: Record<string, BonePose>;
        poses?: Record<string, Record<string, BonePose>>;
        anchorPoses?: Record<string, Record<string, AnchorPose>>;
        anchors?: Record<string, AnchorPose>;
        bindings?: Record<string, BindingPose>;
      };
      boneOverridesByPose = parsed.poses || (parsed.bones ? { [parsed.poseId || selectedPoseId]: parsed.bones } : {});
      anchorOverridesByPose = parsed.anchorPoses || {};
      anchorOverrides = parsed.anchors || {};
      bindingOverrides = parsed.bindings || {};
      importText = raw;
      if (updateStatus) saveStatus = "Imported";
    } catch {
      saveStatus = "Invalid JSON";
    }
  }

  function copyExport() {
    void navigator.clipboard?.writeText(exportText);
    saveStatus = "Copied";
  }

  function storageKey(entryId: string) {
    return `wuxia-mud.animation-rig.${entryId}.v3`;
  }

  function mergedProjectPoseLibrary(rigValue: SkeletonRigDefinition): Record<string, SkeletalPoseDefinition> {
    const poses = clonePoseLibrary(rigValue.poses);
    for (const [poseId, bones] of Object.entries(boneOverridesByPose)) {
      if (!poses[poseId]) continue;
      poses[poseId] = {
        ...poses[poseId],
        bones: {
          ...poses[poseId].bones,
          ...bones
        }
      };
    }
    for (const [poseId, anchors] of Object.entries(anchorOverridesByPose)) {
      if (!poses[poseId]) continue;
      poses[poseId] = {
        ...poses[poseId],
        anchors: {
          ...(poses[poseId].anchors || {}),
          ...anchors
        }
      };
    }
    return poses;
  }

  function clonePoseLibrary(poses: Record<string, SkeletalPoseDefinition>): Record<string, SkeletalPoseDefinition> {
    return JSON.parse(JSON.stringify(poses)) as Record<string, SkeletalPoseDefinition>;
  }

  function makeExportText(entry: AnimationRigEntry | undefined, poseId: string, value: PoseOverrides) {
    return JSON.stringify(
      {
        entryId: entry?.id || null,
        actionId: entry?.actionId || null,
        poseId,
        poses: boneOverridesByPose,
        bones: value.bones,
        anchorPoses: anchorOverridesByPose,
        poseAnchors: currentPoseAnchorOverrides,
        anchors: anchorOverrides,
        bindings: value.bindings
      },
      null,
      2
    );
  }

  function numericInput(event: Event) {
    return Number((event.currentTarget as HTMLInputElement).value);
  }

  function selectInput(event: Event) {
    return (event.currentTarget as HTMLSelectElement).value;
  }

  function optionalSelectInput(event: Event) {
    const value = selectInput(event);
    return value || null;
  }

  function round(value: number) {
    return Math.round(value * 10) / 10;
  }

  function bindingAnchorOptions(rigValue: SkeletonRigDefinition | null) {
    return rigValue?.anchors || [];
  }

  function poseAnchorOptions(rigValue: ResolvedRig | null) {
    return rigValue?.anchors.filter((anchor) => isDeformKeypoint(anchor.id)) || [];
  }

  function isDeformKeypoint(anchorId: string) {
    return !!rig?.bindings.some((binding) => binding.deform?.keypoints.some((keypoint) => keypoint.anchorId === anchorId));
  }

  function keyframePoseIds(rigValue: SkeletonRigDefinition) {
    const preferred = [
      "bind",
      "idle",
      "windup",
      "strike",
      "recover",
      "guard",
      "hurt",
      "dodge",
      "swordReady",
      "swordThrustWindup",
      "swordThrust",
      "swordChopWindup",
      "swordChop",
      "swordLiaoWindup",
      "swordLiao",
      "swordRecover",
      "swordParry",
      "swordHurt",
      "swordDodge"
    ];
    const ordered = preferred.filter((id) => rigValue.poses[id]);
    const remaining = Object.keys(rigValue.poses).filter((id) => !ordered.includes(id));
    return [...ordered, ...remaining];
  }

  function keyframePoses(rigValue: SkeletonRigDefinition) {
    return keyframePoseIds(rigValue).map((id) => rigValue.poses[id]).filter((pose) => !!pose);
  }

  function playbackPose(rigValue: SkeletonRigDefinition, timeMs: number, selectedId: string) {
    const poseIds = playbackPoseIds(rigValue, selectedId);
    if (poseIds.length < 2) return rigValue.poses[selectedId] || rigValue.poses.idle || Object.values(rigValue.poses)[0];
    const segments = poseIds.slice(1).map((id, index) => {
      const from = rigValue.poses[poseIds[index]];
      const to = rigValue.poses[id];
      return {
        from,
        to,
        duration: Math.max(80, to?.durationMs || from?.durationMs || 240)
      };
    });
    const total = segments.reduce((sum, segment) => sum + segment.duration, 0);
    let cursor = total > 0 ? timeMs % total : 0;
    for (const segment of segments) {
      if (cursor <= segment.duration) {
        const t = easeInOut(cursor / segment.duration);
        return interpolatePose(segment.from, segment.to, t, "playback");
      }
      cursor -= segment.duration;
    }
    return segments[segments.length - 1].to;
  }

  function playbackPoseIds(rigValue: SkeletonRigDefinition, selectedId: string) {
    const swordSequences: Record<string, string[]> = {
      swordThrust: ["swordReady", "swordThrustWindup", "swordThrust", "swordRecover", "swordReady"],
      swordThrustWindup: ["swordReady", "swordThrustWindup", "swordThrust", "swordRecover", "swordReady"],
      swordChop: ["swordReady", "swordChopWindup", "swordChop", "swordRecover", "swordReady"],
      swordChopWindup: ["swordReady", "swordChopWindup", "swordChop", "swordRecover", "swordReady"],
      swordLiao: ["swordReady", "swordLiaoWindup", "swordLiao", "swordRecover", "swordReady"],
      swordLiaoWindup: ["swordReady", "swordLiaoWindup", "swordLiao", "swordRecover", "swordReady"],
      swordParry: ["swordReady", "swordParry", "swordReady"],
      swordHurt: ["swordReady", "swordHurt", "swordReady"],
      swordDodge: ["swordReady", "swordDodge", "swordReady"]
    };
    const swordOrder = swordSequences[selectedId]?.filter((id) => rigValue.poses[id]);
    if (swordOrder && swordOrder.length >= 2) return swordOrder;
    if (selectedId.startsWith("sword") && selectedId !== "swordReady" && rigValue.poses.swordReady && rigValue.poses[selectedId]) {
      return ["swordReady", selectedId, "swordReady"];
    }
    const segmentedOrder = ["bind", "idle", "windup", "strike", "recover", "idle"].filter((id) => rigValue.poses[id]);
    if (segmentedOrder.length >= 2) return segmentedOrder;
    const idle = rigValue.poses.idle ? "idle" : Object.keys(rigValue.poses)[0];
    if (!idle) return [];
    if (selectedId && selectedId !== idle && rigValue.poses[selectedId]) return [idle, selectedId, idle];
    return [idle, ...Object.keys(rigValue.poses).filter((id) => id !== idle).slice(0, 1), idle].filter(Boolean);
  }

  function easeInOut(value: number) {
    const t = Math.max(0, Math.min(1, value));
    return t * t * (3 - 2 * t);
  }
</script>

<svelte:head>
  <title>Animation Rig Tool</title>
</svelte:head>

<div class="rig-tool-shell">
  <header class="rig-tool-header">
    <div>
      <h1>骨骼动画工具</h1>
      <p>{entries.length} action/profile entries</p>
    </div>
    <div class="rig-header-meta">
      <span>{selectedEntry?.actionId}</span>
      <span>{selectedEntry?.profile}</span>
      <span>{selectedEntry?.style}</span>
    </div>
  </header>

  <main class="rig-tool-layout">
    <aside class="rig-list-panel">
      <div class="rig-filter-row">
        <input aria-label="Search animations" placeholder="Search action, clip, tag" bind:value={search} />
      </div>
      <div class="rig-filter-grid">
        <select bind:value={profileFilter} aria-label="Profile filter">
          <option value="all">All profiles</option>
          <option value="male">Male</option>
          <option value="female">Female</option>
        </select>
        <select bind:value={styleFilter} aria-label="Style filter">
          <option value="all">All styles</option>
          <option value="sword">Sword</option>
          <option value="fist">Fist</option>
        </select>
      </div>

      <div class="rig-entry-list" aria-label="Animation entries">
        {#each filteredEntries as entry (entry.id)}
          <button type="button" class:active={entry.id === selectedEntryId} on:click={() => selectEntry(entry.id)}>
            <strong>{entry.actionId}</strong>
            <span>{entry.profile} / {entry.style} / {entry.poseId}</span>
            <small>{entry.clipId || "no clip"} · {entry.durationMs}ms</small>
          </button>
        {/each}
      </div>
    </aside>

    <section class="rig-workbench">
      <div class="rig-toolbar">
        <div class="rig-segmented">
          <button type="button" class:active={dragMode === "pose"} on:click={() => (dragMode = "pose")}>Pose</button>
          <button type="button" class:active={dragMode === "anchor"} on:click={() => (dragMode = "anchor")}>Anchor</button>
          <button type="button" class:active={dragMode === "binding"} on:click={() => (dragMode = "binding")}>Bind</button>
        </div>
        <div class="rig-segmented">
          <button type="button" on:click={() => stepPose(-1)}>Prev</button>
          <button type="button" on:click={() => stepPose(1)}>Next</button>
          <button type="button" class:active={isPlaying} on:click={togglePlayback}>{isPlaying ? "Pause" : "Play Preview"}</button>
          <button type="button" on:click={resetPlayback}>Restart</button>
        </div>

        <label><input type="checkbox" bind:checked={showImages} /> Source</label>
        <label><input type="checkbox" bind:checked={showSkin} /> Skin</label>
        <label><input type="checkbox" bind:checked={showBones} /> Skeleton</label>
        <label><input type="checkbox" bind:checked={showAnchors} /> Anchors</label>
        <label><input type="checkbox" bind:checked={showBindings} /> Bindings</label>
        <label><input type="checkbox" bind:checked={showLabels} /> Labels</label>
        <label><input type="checkbox" bind:checked={lockLimbLengths} /> Lock lengths</label>
        <label class="rig-zoom">Zoom <input type="range" min="0.45" max="7" step="0.05" value={zoom} on:input={handleZoomInput} /></label>
        <button type="button" class="rig-secondary" on:click={resetView}>Reset View</button>
      </div>

      <canvas
        class="rig-canvas"
        bind:this={canvasEl}
        on:wheel|nonpassive={handleCanvasWheel}
        on:pointerdown={handlePointerDown}
        on:pointermove={handlePointerMove}
        on:pointerup={handlePointerUp}
        on:pointercancel={handlePointerUp}
      ></canvas>

      <div class="rig-status-row">
        <span>Anchor: {selectedAnchor?.id || "-"}</span>
        <span>Pose bone: {selectedBone?.id || "-"}</span>
        <span>Binding: {selectedBinding?.id || "-"}</span>
        <span>View: {zoom.toFixed(2)}x / {Math.round(viewportPan.x)}, {Math.round(viewportPan.y)}</span>
      </div>
    </section>

    <aside class="rig-inspector">
      <section>
        <div class="rig-section-head">
          <h2>Pose</h2>
          <select value={selectedPoseId} aria-label="Pose preset" on:change={(event) => selectPose(selectInput(event))}>
            {#if rig}
              {#each keyframePoses(rig) as pose (pose.id)}
                <option value={pose.id}>{pose.name}</option>
              {/each}
            {/if}
          </select>
        </div>

        <label class="rig-field">
          Pose bone
          <select bind:value={selectedBoneId}>
            {#each resolvedRig?.bones || [] as bone (bone.id)}
              <option value={bone.id}>{bone.id}</option>
            {/each}
          </select>
        </label>

        <div class="rig-slider-field">
          <span>Rotation</span>
          <input
            type="range"
            min="-180"
            max="180"
            step="1"
            value={selectedBone?.local.rotation ?? 0}
            on:input={(event) => updateSelectedBone({ rotation: numericInput(event) })}
          />
          <input
            type="number"
            step="1"
            value={round(selectedBone?.local.rotation ?? 0)}
            on:input={(event) => updateSelectedBone({ rotation: numericInput(event) })}
          />
        </div>

        <div class="rig-pair-fields">
          <label>
            X
            <input type="number" step="1" value={round(selectedBone?.local.x ?? 0)} on:input={(event) => updateSelectedBone({ x: numericInput(event) })} />
          </label>
          <label>
            Y
            <input type="number" step="1" value={round(selectedBone?.local.y ?? 0)} on:input={(event) => updateSelectedBone({ y: numericInput(event) })} />
          </label>
        </div>

        <label class="rig-field">
          Keypoint
          <select bind:value={selectedAnchorId}>
            {#each poseAnchorOptions(resolvedRig) as anchor (anchor.id)}
              <option value={anchor.id}>{anchor.id}</option>
            {/each}
          </select>
        </label>

        <div class="rig-pair-fields">
          <label>
            Key X
            <input
              type="number"
              step="0.5"
              value={round(selectedAnchor?.definition.x ?? 0)}
              on:input={(event) => updateSelectedPoseAnchor({ x: numericInput(event) })}
            />
          </label>
          <label>
            Key Y
            <input
              type="number"
              step="0.5"
              value={round(selectedAnchor?.definition.y ?? 0)}
              on:input={(event) => updateSelectedPoseAnchor({ y: numericInput(event) })}
            />
          </label>
        </div>

        <button type="button" class="rig-secondary" on:click={resetSelectedPoseAnchor}>Reset Keypoint</button>
        <button type="button" class="rig-secondary" on:click={resetSelectedBone}>Reset Bone</button>
      </section>

      <section>
        <div class="rig-section-head">
          <h2>Anchor</h2>
          <button type="button" class="rig-secondary" on:click={resetSelectedAnchor}>Reset</button>
        </div>

        <label class="rig-field">
          Anchor
          <select bind:value={selectedAnchorId}>
            {#each resolvedRig?.anchors || [] as anchor (anchor.id)}
              <option value={anchor.id}>{anchor.id}</option>
            {/each}
          </select>
        </label>

        <label class="rig-field">
          Bone
          <select value={selectedAnchor?.definition.boneId || ""} on:change={(event) => updateSelectedAnchor({ boneId: selectInput(event) })}>
            {#each resolvedRig?.bones || [] as bone (bone.id)}
              <option value={bone.id}>{bone.id}</option>
            {/each}
          </select>
        </label>

        <label class="rig-field">
          Drag handle
          <select
            value={selectedAnchor?.definition.handleBoneId || ""}
            on:change={(event) => updateSelectedAnchor({ handleBoneId: optionalSelectInput(event) })}
          >
            <option value="">None</option>
            {#each resolvedRig?.bones || [] as bone (bone.id)}
              <option value={bone.id}>{bone.id}</option>
            {/each}
          </select>
        </label>

        <div class="rig-pair-fields">
          <label>
            Anchor X
            <input
              type="number"
              step="0.5"
              value={round(selectedAnchor?.definition.x ?? 0)}
              on:input={(event) => updateSelectedAnchor({ x: numericInput(event) })}
            />
          </label>
          <label>
            Anchor Y
            <input
              type="number"
              step="0.5"
              value={round(selectedAnchor?.definition.y ?? 0)}
              on:input={(event) => updateSelectedAnchor({ y: numericInput(event) })}
            />
          </label>
        </div>
      </section>

      <section>
        <div class="rig-section-head">
          <h2>Binding</h2>
          <button type="button" class="rig-secondary" on:click={resetSelectedBinding}>Reset</button>
        </div>

        <label class="rig-field">
          Binding
          <select bind:value={selectedBindingId}>
            {#each resolvedRig?.bindings || [] as binding (binding.id)}
              <option value={binding.id}>{binding.id}</option>
            {/each}
          </select>
        </label>

        <label class="rig-field">
          Anchor
          <select value={selectedBinding?.definition.anchorId || ""} on:change={(event) => updateSelectedBinding({ anchorId: selectInput(event) })}>
            {#each bindingAnchorOptions(rig) as anchor (anchor.id)}
              <option value={anchor.id}>{anchor.id}</option>
            {/each}
          </select>
        </label>

        <div class="rig-pair-fields">
          <label>
            Offset X
            <input
              type="number"
              step="0.5"
              value={round(selectedBinding?.definition.offsetX ?? 0)}
              on:input={(event) => updateSelectedBinding({ offsetX: numericInput(event) })}
            />
          </label>
          <label>
            Offset Y
            <input
              type="number"
              step="0.5"
              value={round(selectedBinding?.definition.offsetY ?? 0)}
              on:input={(event) => updateSelectedBinding({ offsetY: numericInput(event) })}
            />
          </label>
        </div>

        <div class="rig-pair-fields">
          <label>
            Rotation
            <input
              type="number"
              step="1"
              value={round(selectedBinding?.definition.rotation ?? 0)}
              on:input={(event) => updateSelectedBinding({ rotation: numericInput(event) })}
            />
          </label>
          <label>
            Opacity
            <input
              type="number"
              min="0"
              max="1"
              step="0.05"
              value={round(selectedBinding?.definition.opacity ?? 1)}
              on:input={(event) => updateSelectedBinding({ opacity: numericInput(event) })}
            />
          </label>
        </div>

        <div class="rig-pair-fields">
          <label>
            Scale X
            <input
              type="number"
              step="0.05"
              value={round(selectedBinding?.definition.scaleX ?? 1)}
              on:input={(event) => updateSelectedBinding({ scaleX: numericInput(event) })}
            />
          </label>
          <label>
            Scale Y
            <input
              type="number"
              step="0.05"
              value={round(selectedBinding?.definition.scaleY ?? 1)}
              on:input={(event) => updateSelectedBinding({ scaleY: numericInput(event) })}
            />
          </label>
        </div>
      </section>

      <section>
        <div class="rig-section-head">
          <h2>Anchor Map</h2>
          <span>{resolvedRig?.anchors.length || 0}</span>
        </div>
        <div class="rig-anchor-table">
          {#each resolvedRig?.anchors || [] as anchor (anchor.id)}
            <button
              type="button"
              class:active={anchor.id === selectedAnchorId}
              on:click={() => {
                selectedAnchorId = anchor.id;
                if (anchor.definition.handleBoneId) selectedBoneId = anchor.definition.handleBoneId;
              }}
            >
              <strong>{anchor.id}</strong>
              <span>{anchor.bindingIds.join(", ") || "-"}</span>
            </button>
          {/each}
        </div>
      </section>

      <section>
        <div class="rig-section-head">
          <h2>JSON</h2>
          <span>{saveStatus}</span>
        </div>
        <div class="rig-command-row">
          <button type="button" class="rig-primary" on:click={saveEdits}>Save Local</button>
          <button type="button" class="rig-primary" disabled={!canSaveProjectPoses} on:click={saveProjectPoses}>Save Project Poses</button>
          <button type="button" class="rig-secondary" on:click={copyExport}>Copy</button>
          <button type="button" class="rig-secondary" on:click={clearSavedEdits}>Clear</button>
          <button type="button" class="rig-secondary" on:click={resetAll}>Reset All</button>
        </div>
        <textarea readonly value={exportText}></textarea>
        <textarea placeholder="Paste overrides JSON" bind:value={importText}></textarea>
        <button type="button" class="rig-secondary" on:click={() => applyImport()}>Import JSON</button>
      </section>
    </aside>
  </main>
</div>
