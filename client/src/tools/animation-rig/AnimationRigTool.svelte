<script lang="ts">
  import { onMount } from "svelte";
  import { parse as parseYaml, stringify as stringifyYaml } from "yaml";
  import { createBattleActorRig, staticSkeletalAnimationEntries } from "../../battle/skeletal/catalog";
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
  import type { CombatStyle, TargetReaction } from "../../battle/animationTypes";
  import type { ActorMotion, BattleActionDefinition } from "../../battle/animationTypes";
  import type { CombatResult } from "../../protocol";

  type ToolTab = "actions" | "mapping";
  type LibraryTab = "actions" | "poses";
  type InspectorTab = "action" | "feedback" | "pose" | "anchor" | "binding" | "map" | "io";
  type StyleFilter = CombatStyle | "all";
  type DragMode = "pose" | "anchor" | "binding";
  type RigActionManifest = { schemaVersion: number; actions: BattleActionDefinition[] };
  type ToolSelectionSnapshot = {
    activeTab?: ToolTab;
    activeLibraryTab?: LibraryTab;
    activeInspectorTab?: InspectorTab;
    selectedRigActionId?: string;
    selectedEntryId?: string;
    selectedPoseId?: string;
    selectedBindingId?: string;
    selectedAnchorId?: string;
    selectedBoneId?: string;
    dragMode?: DragMode;
  };
  type MartialArtFile = { file: string; text: string };
  type YamlObject = Record<string, any>;
  type MartialParseResult = { root: unknown; arts: ParsedMartialArt[]; error: string };
  type ParsedMartialArt = { key: string; index: number; data: YamlObject; id: string; name: string };
  type MartialTarget = { key: string; kind: "attack_moves" | "active_skills"; index: number; data: YamlObject; id: string; name: string };
  type DragState =
    | { mode: "pose"; boneId: string }
    | { mode: "poseAnchor"; anchorId: string }
    | { mode: "anchor"; anchorId: string }
    | { mode: "binding"; bindingId: string };

  const staticRigEntries = staticSkeletalAnimationEntries;
  const visualProfiles: AnimationRigEntry["profile"][] = ["male", "female"];
  const combatResultLabels: Record<CombatResult, string> = { hit: "命中", dodge: "闪避", parry: "招架", effect: "效果" };
  const targetReactionLabels: Record<TargetReaction, string> = { none: "无目标反应", hit: "受击", dodge: "闪避", parry: "招架", effect: "效果反应" };
  const vfxKindLabels: Record<BattleActionDefinition["vfx"][number]["kind"], string> = {
    trail: "轨迹",
    impact: "命中特效",
    parry: "招架特效",
    aura: "气场",
    heal: "治疗"
  };
  const vfxAnchorLabels: Record<BattleActionDefinition["vfx"][number]["anchor"], string> = { actor: "出招者", target: "目标", center: "场中央" };
  const reactionResults: CombatResult[] = ["hit", "dodge", "parry", "effect"];
  const dragModeLabels: Record<DragMode, string> = {
    pose: "骨骼",
    anchor: "锚点",
    binding: "贴图"
  };
  const dragModeTitles: Record<DragMode, string> = {
    pose: "骨骼模式：拖动画布上的骨骼或姿势控制点，修改当前关键姿势",
    anchor: "锚点模式：拖动挂在骨骼上的命名锚点",
    binding: "贴图模式：拖动图片部件相对锚点的绑定位置"
  };
  const editableBindingKinds: NonNullable<BindingPose["kind"]>[] = ["image", "capsule", "circle", "line", "target"];
  const bindingKindLabels: Record<string, string> = {
    image: "图片",
    mesh: "蒙皮图片",
    capsule: "胶囊",
    circle: "圆形",
    line: "线条",
    target: "十字标记"
  };
  const selectionStorageKey = "wuxia-mud.animation-rig.selection.v1";
  const initialSelection = loadToolSelection();

  let canvasEl: HTMLCanvasElement;
  let renderer: SkeletalCanvasRenderer | null = null;
  let viewport: RigViewport | null = null;
  let resizeObserver: ResizeObserver | null = null;
  let drawFrame = 0;

  let activeTab: ToolTab = initialSelection.activeTab || "actions";
  let activeLibraryTab: LibraryTab = initialSelection.activeLibraryTab || "actions";
  let activeInspectorTab: InspectorTab = initialSelection.activeInspectorTab || "action";
  let selectedEntryId = initialSelection.selectedEntryId || "";
  let selectedPoseId = initialSelection.selectedPoseId || "";
  let poseIdDraft = selectedPoseId;
  let lastPoseIdDraftSource = selectedPoseId;
  let poseNameDraft = "";
  let poseDurationDraft = 240;
  let lastPoseMetadataDraftSource = "";
  let lastEntryId = "";
  let styleFilter: StyleFilter = "all";
  let search = "";
  let dragMode: DragMode = initialSelection.dragMode || "pose";
  let dragState: DragState | null = null;
  let selectedBoneId = initialSelection.selectedBoneId || "frontArm";
  let selectedAnchorId = initialSelection.selectedAnchorId || "frontWrist";
  let selectedBindingId = initialSelection.selectedBindingId || "part.arm_front";
  let isRestoringToolSelection = true;
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
  let bindingOverridesByPose: Record<string, Record<string, BindingPose>> = {};
  let anchorOverrides: Record<string, AnchorPose> = {};
  let bindingOverrides: Record<string, BindingPose> = {};
  let importText = "";
  let saveStatus = "";
  let projectPoseLibrary: Record<string, SkeletalPoseDefinition> | null = null;
  let rigActionManifest: RigActionManifest = { schemaVersion: 1, actions: [] };
  let selectedRigActionId = initialSelection.selectedRigActionId || "";
  let rigActionStatus = "";
  let martialFiles: MartialArtFile[] = [];
  let selectedMartialFile = "";
  let martialStatus = "";
  let selectedMartialArtKey = "";
  let selectedMappingTargetKey = "";
  let selectedMappingPoolId = "basic";

  $: entries = [...entriesForRigActions(rigActionManifest.actions), ...staticRigEntries];
  $: selectedRigAction = rigActionManifest.actions.find((action) => action.id === selectedRigActionId) || rigActionManifest.actions[0];
  $: selectedActionEntries = selectedRigAction ? entries.filter((entry) => entry.actionId === selectedRigAction.id) : [];
  $: if (selectedRigAction && selectedActionEntries.length > 0 && !selectedActionEntries.some((entry) => entry.id === selectedEntryId)) {
    selectedEntryId = selectedActionEntries[0].id;
  }
  $: selectedEntry = selectedActionEntries.find((entry) => entry.id === selectedEntryId);
  $: filteredRigActions = filterRigActions(rigActionManifest.actions, styleFilter, search);
  $: actionPoseOptions = rig ? keyframePoses(rig) : [];
  $: filteredPoseOptions = filterPoseOptions(actionPoseOptions, rigActionManifest.actions, search);
  $: if (selectedPoseId !== lastPoseIdDraftSource) {
    poseIdDraft = selectedPoseId;
    lastPoseIdDraftSource = selectedPoseId;
  }
  $: selectedActionSequence = selectedRigAction ? normalizedActionSequence(selectedRigAction) : [];
  $: selectedActionPoseIds = selectedRigAction ? uniqueTextList([selectedRigAction.poseId, ...selectedActionSequence]) : [];
  $: missingActionPoseIds =
    selectedRigAction && rig ? uniqueTextList(selectedActionPoseIds.filter((poseId) => !rig.poses[poseId])) : [];
  $: selectedMartial = martialFiles.find((file) => file.file === selectedMartialFile) || martialFiles[0];
  $: martialParse = parseMartialFile(selectedMartial?.text || "");
  $: currentMartialArt = martialParse.arts.find((art) => art.key === selectedMartialArtKey) || martialParse.arts[0];
  $: mappingTargets = currentMartialArt ? buildMartialTargets(currentMartialArt.data) : [];
  $: currentMappingTarget = mappingTargets.find((target) => target.key === selectedMappingTargetKey) || mappingTargets[0];
  $: currentPoolId = currentMartialArt ? resolvedPoolId(currentMartialArt.data) : "";
  $: currentPoolEntries = currentMartialArt && currentPoolId ? poolEntries(currentMartialArt.data, currentPoolId) : [];
  $: rig = selectedEntry ? createRigWithProjectPoses(selectedEntry, projectPoseLibrary) : null;
  $: if (selectedEntry && selectedEntry.id !== lastEntryId) {
    lastEntryId = selectedEntry.id;
    if (!selectedPoseId) selectedPoseId = selectedEntry.poseId;
    isPlaying = false;
    playbackTime = 0;
    playbackLastMs = 0;
    const defaultBoneId = selectedEntry.tags.includes("part-rig") ? "frontArm" : "frontForearm";
    const defaultAnchorId = selectedEntry.tags.includes("part-rig") ? "frontWrist" : "frontHand";
    const defaultBindingId = selectedEntry.tags.includes("part-rig") ? "part.arm_front" : "source.frame";
    if (isRestoringToolSelection) {
      selectedBoneId = selectedBoneId || defaultBoneId;
      selectedAnchorId = selectedAnchorId || defaultAnchorId;
      selectedBindingId = selectedBindingId || defaultBindingId;
    } else {
      selectedBoneId = defaultBoneId;
      selectedAnchorId = defaultAnchorId;
      selectedBindingId = defaultBindingId;
    }
    loadSavedEdits(selectedEntry.id);
    isRestoringToolSelection = false;
  }
  $: selectedPose = rig?.poses[selectedPoseId] || (selectedEntry && rig?.poses[selectedEntry.poseId]) || rig?.poses.idle;
  $: {
    const metadataSource = `${selectedPoseId}:${selectedPose?.name || ""}:${selectedPose?.durationMs || 240}`;
    if (metadataSource !== lastPoseMetadataDraftSource) {
      poseNameDraft = selectedPose?.name || selectedPoseId;
      poseDurationDraft = selectedPose?.durationMs || 240;
      lastPoseMetadataDraftSource = metadataSource;
    }
  }
  $: activePose = rig && isPlaying ? playbackPose(rig, playbackTime, selectedPoseId, selectedRigAction?.sequence || []) : selectedPose;
  $: currentBoneOverrides = isPlaying ? {} : boneOverridesByPose[selectedPoseId] || {};
  $: currentPoseAnchorOverrides = isPlaying ? {} : anchorOverridesByPose[selectedPoseId] || {};
  $: currentPoseBindingOverrides = isPlaying ? {} : bindingOverridesByPose[selectedPoseId] || {};
  $: currentAnchorOverrides = { ...anchorOverrides, ...currentPoseAnchorOverrides };
  $: currentBindingOverrides = { ...bindingOverrides, ...currentPoseBindingOverrides };
  $: overrides = { bones: currentBoneOverrides, anchors: currentAnchorOverrides, bindings: currentBindingOverrides };
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
    void loadProjectPoses();
    void loadRigActions();
    void loadMartialArts();
    queueDraw();
    return () => {
      resizeObserver?.disconnect();
      if (drawFrame) cancelAnimationFrame(drawFrame);
    };
  });

  function loadToolSelection(): ToolSelectionSnapshot {
    if (typeof localStorage === "undefined") return {};
    try {
      const parsed = JSON.parse(localStorage.getItem(selectionStorageKey) || "{}") as ToolSelectionSnapshot;
      return {
        activeTab: parsed.activeTab === "actions" || parsed.activeTab === "mapping" ? parsed.activeTab : undefined,
        activeLibraryTab: parsed.activeLibraryTab === "actions" || parsed.activeLibraryTab === "poses" ? parsed.activeLibraryTab : undefined,
        activeInspectorTab: isInspectorTab(parsed.activeInspectorTab) ? parsed.activeInspectorTab : undefined,
        selectedRigActionId: parsed.selectedRigActionId || undefined,
        selectedEntryId: parsed.selectedEntryId || undefined,
        selectedPoseId: parsed.selectedPoseId || undefined,
        selectedBindingId: parsed.selectedBindingId || undefined,
        selectedAnchorId: parsed.selectedAnchorId || undefined,
        selectedBoneId: parsed.selectedBoneId || undefined,
        dragMode: isDragMode(parsed.dragMode) ? parsed.dragMode : undefined
      };
    } catch {
      return {};
    }
  }

  function persistToolSelection() {
    if (typeof localStorage === "undefined") return;
    const snapshot: ToolSelectionSnapshot = {
      activeTab,
      activeLibraryTab,
      activeInspectorTab,
      selectedRigActionId: selectedRigAction?.id || selectedRigActionId,
      selectedEntryId: selectedEntry?.id || selectedEntryId,
      selectedPoseId,
      selectedBindingId,
      selectedAnchorId,
      selectedBoneId,
      dragMode
    };
    localStorage.setItem(selectionStorageKey, JSON.stringify(snapshot));
  }

  function isInspectorTab(value: unknown): value is InspectorTab {
    return value === "action" || value === "feedback" || value === "pose" || value === "anchor" || value === "binding" || value === "map" || value === "io";
  }

  function isDragMode(value: unknown): value is DragMode {
    return value === "pose" || value === "anchor" || value === "binding";
  }

  function filterRigActions(list: BattleActionDefinition[], style: StyleFilter, term: string) {
    const q = term.trim().toLowerCase();
    return list.filter((action) => {
      if (style !== "all" && action.style !== style) return false;
      if (!q) return true;
      return [action.id, action.label, action.style, action.actorMotion, action.poseId, ...action.tags].join(" ").toLowerCase().includes(q);
    });
  }

  function filterPoseOptions(list: SkeletalPoseDefinition[], actions: BattleActionDefinition[], term: string) {
    const q = term.trim().toLowerCase();
    if (!q) return list;
    return list.filter((pose) => {
      const users = actions.filter((action) => actionUsesPose(action, pose.id)).map((action) => action.id);
      return [pose.id, pose.name, String(pose.durationMs || ""), ...users].join(" ").toLowerCase().includes(q);
    });
  }

  function entriesForRigActions(actions: BattleActionDefinition[]): AnimationRigEntry[] {
    return actions.flatMap((action) =>
      visualProfiles.map((profile) => ({
        id: `${action.id}.${profile}`,
        actionId: action.id,
        clipId: action.rig,
        label: `${action.label} / ${profile}`,
        profile,
        style: action.style,
        poseId: action.poseId,
        sprite: null,
        tags: ["part-rig", "v12", profile, action.style, ...action.tags],
        durationMs: action.durationMs
      }))
    );
  }

  function createRigWithProjectPoses(entry: AnimationRigEntry, poseLibrary: Record<string, SkeletalPoseDefinition> | null) {
    return createBattleActorRig(entry, poseLibrary || {});
  }

  async function loadProjectPoses() {
    try {
      const response = await fetch("/__rig/segmented-v12-poses");
      if (!response.ok) throw new Error(await response.text());
      projectPoseLibrary = (await response.json()) as Record<string, SkeletalPoseDefinition>;
    } catch (error) {
      saveStatus = error instanceof Error ? `Load poses failed: ${error.message}` : "Load poses failed";
    }
  }

  function selectEntry(id: string) {
    selectedEntryId = id;
    const entry = entries.find((candidate) => candidate.id === id);
    if (entry && rigActionManifest.actions.some((action) => action.id === entry.actionId)) selectedRigActionId = entry.actionId;
  }

  function selectRigAction(id: string) {
    selectedRigActionId = id;
    const preferredProfile = selectedEntry?.profile;
    const entry =
      entries.find((candidate) => candidate.actionId === id && (!preferredProfile || candidate.profile === preferredProfile)) ||
      entries.find((candidate) => candidate.actionId === id);
    if (entry) {
      selectedEntryId = entry.id;
      selectedPoseId = entry.poseId;
      isPlaying = false;
      playbackTime = 0;
      playbackLastMs = 0;
    }
  }

  function entryCountForAction(actionId: string) {
    return entries.filter((entry) => entry.actionId === actionId).length;
  }

  function actionUsesPose(action: BattleActionDefinition, poseId: string) {
    return action.poseId === poseId || action.sequence.includes(poseId);
  }

  function poseUsageCount(poseId: string) {
    return rigActionManifest.actions.filter((action) => actionUsesPose(action, poseId)).length;
  }

  function poseUsageSummary(poseId: string) {
    const users = rigActionManifest.actions.filter((action) => actionUsesPose(action, poseId)).map((action) => action.id);
    if (!users.length) return "未被动作引用";
    return users.slice(0, 3).join(", ") + (users.length > 3 ? ` +${users.length - 3}` : "");
  }

  function previewEntryLabel(entry: AnimationRigEntry) {
    return `${entry.profile} / ${entry.style} / ${entry.poseId}`;
  }

  function uniqueTextList(values: string[]) {
    return Array.from(new Set(values.map((value) => value.trim()).filter(Boolean)));
  }

  function poseLabelById(id: string, rigValue: SkeletonRigDefinition | null = rig) {
    return rigValue?.poses[id]?.name || id;
  }

  function poseOptionLabel(id: string, rigValue: SkeletonRigDefinition | null = rig) {
    const pose = rigValue?.poses[id];
    return pose ? `${pose.name} / ${id}` : `缺失姿势 / ${id}`;
  }

  function normalizedActionSequence(action: BattleActionDefinition) {
    return action.sequence.length ? action.sequence : [action.poseId];
  }

  function setPrimaryPoseId(poseId: string) {
    if (!selectedRigAction || !poseId) return;
    const previousPoseId = selectedRigAction.poseId;
    const sequence = selectedActionSequence;
    const previousIndex = sequence.indexOf(previousPoseId);
    const nextSequence = [...sequence];
    if (previousIndex >= 0) {
      nextSequence[previousIndex] = poseId;
    } else if (!nextSequence.includes(poseId)) {
      nextSequence.unshift(poseId);
    }
    updateRigAction({ poseId, sequence: nextSequence });
    selectPose(poseId);
  }

  function updateSequencePose(index: number, poseId: string) {
    if (!selectedRigAction || !poseId) return;
    const sequence = selectedActionSequence;
    if (index < 0 || index >= sequence.length) return;
    const nextSequence = sequence.map((candidate, candidateIndex) => (candidateIndex === index ? poseId : candidate));
    updateRigAction({ sequence: nextSequence });
    selectPose(poseId);
  }

  function addSequencePose(afterIndex: number) {
    if (!selectedRigAction) return;
    const fallbackPoseId = selectedRigAction.poseId || actionPoseOptions[0]?.id;
    if (!fallbackPoseId) return;
    const sequence = selectedActionSequence;
    const insertAt = Math.max(0, Math.min(sequence.length, afterIndex + 1));
    updateRigAction({ sequence: [...sequence.slice(0, insertAt), fallbackPoseId, ...sequence.slice(insertAt)] });
  }

  function removeSequencePose(index: number) {
    if (!selectedRigAction) return;
    const sequence = selectedActionSequence;
    const nextSequence = sequence.filter((_, candidateIndex) => candidateIndex !== index);
    updateRigAction({ sequence: nextSequence.length ? nextSequence : [selectedRigAction.poseId] });
  }

  function moveSequencePose(index: number, direction: -1 | 1) {
    if (!selectedRigAction) return;
    const sequence = selectedActionSequence;
    const nextIndex = index + direction;
    if (nextIndex < 0 || nextIndex >= sequence.length) return;
    const nextSequence = [...sequence];
    [nextSequence[index], nextSequence[nextIndex]] = [nextSequence[nextIndex], nextSequence[index]];
    updateRigAction({ sequence: nextSequence });
  }

  function defaultTargetReaction(result: CombatResult): TargetReaction {
    return result;
  }

  function targetReactionFor(result: CombatResult) {
    return selectedRigAction?.targetReaction[result] || defaultTargetReaction(result);
  }

  function vfxSummary(vfx: BattleActionDefinition["vfx"][number]) {
    return `${vfxKindLabels[vfx.kind]} / ${vfx.variant} / ${vfxAnchorLabels[vfx.anchor]}`;
  }

  function selectPose(id: string) {
    selectedPoseId = id;
    isPlaying = false;
    playbackTime = 0;
    playbackLastMs = 0;
    queueDraw();
  }

  function selectPoseFromLibrary(id: string) {
    selectPose(id);
    activeInspectorTab = "pose";
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
      meshQuality: isPlaying ? "fast" : "full",
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
    setPoseBindingOverride(bindingId, { offsetX: round(local.x), offsetY: round(local.y) });
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

  function setPoseBindingOverride(bindingId: string, patch: BindingPose) {
    isPlaying = false;
    const poseOverrides = bindingOverridesByPose[selectedPoseId] || {};
    bindingOverridesByPose = {
      ...bindingOverridesByPose,
      [selectedPoseId]: {
        ...poseOverrides,
        [bindingId]: compactBindingPose({ ...(poseOverrides[bindingId] || {}), ...patch })
      }
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
    setPoseBindingOverride(selectedBindingId, patch);
  }

  function selectWeaponBinding() {
    if (!resolvedRig?.bindingsById["prop.sword"]) return;
    selectedBindingId = "prop.sword";
    activeInspectorTab = "binding";
    dragMode = "binding";
  }

  function hideSelectedBindingForCurrentPose() {
    if (!selectedBindingId) return;
    updateSelectedBinding({ opacity: 0 });
    saveStatus = `Hidden ${selectedBindingId} in ${selectedPoseId}`;
  }

  function hideSelectedBindingForAllPoses() {
    if (!selectedBindingId) return;
    updateSelectedBindingForAllProjectPoses(selectedBindingId, { opacity: 0 });
    saveStatus = `Hidden ${selectedBindingId} in all poses`;
  }

  function applySwordBindingTemplate() {
    if (!selectedBindingId) return;
    updateSelectedBinding({
      kind: "line",
      width: 82,
      height: 2.1,
      color: "rgba(214, 222, 214, 0.95)",
      strokeColor: "rgba(96, 112, 105, 0.6)",
      opacity: 0.95,
      pivotX: 0,
      pivotY: 0.5
    });
    saveStatus = `Updated ${selectedBindingId} shape`;
  }

  function updateSelectedBindingForAllProjectPoses(bindingId: string, patch: BindingPose) {
    const library = currentProjectPoseLibrary();
    projectPoseLibrary = Object.fromEntries(
      Object.entries(library).map(([poseId, pose]) => [
        poseId,
        {
          ...pose,
          bindings: {
            ...(pose.bindings || {}),
            [bindingId]: compactBindingPose({ ...(pose.bindings?.[bindingId] || {}), ...patch })
          }
        }
      ])
    );
    const { [bindingId]: _globalBinding, ...globalRest } = bindingOverrides;
    const nextBindingOverridesByPose = { ...bindingOverridesByPose };
    for (const [poseId, poseOverrides] of Object.entries(nextBindingOverridesByPose)) {
      const { [bindingId]: _poseBinding, ...poseRest } = poseOverrides;
      nextBindingOverridesByPose[poseId] = poseRest;
    }
    bindingOverrides = globalRest;
    bindingOverridesByPose = nextBindingOverridesByPose;
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
    const poseOverrides = bindingOverridesByPose[selectedPoseId] || {};
    const { [selectedBindingId]: _poseBinding, ...poseRest } = poseOverrides;
    const { [selectedBindingId]: _globalBinding, ...globalRest } = bindingOverrides;
    bindingOverridesByPose = { ...bindingOverridesByPose, [selectedPoseId]: poseRest };
    bindingOverrides = globalRest;
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
    bindingOverridesByPose = {};
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
      persistToolSelection();
      const poses = mergedProjectPoseLibrary(rig);
      const response = await fetch("/__rig/segmented-v12-poses", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ poses })
      });
      if (!response.ok) throw new Error(await response.text());
      projectPoseLibrary = poses;
      localStorage.removeItem(storageKey(selectedEntry.id));
      resetAll();
      saveStatus = "Saved pose library";
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
      bindingOverridesByPose = {};
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
        bindingPoses?: Record<string, Record<string, BindingPose>>;
        anchors?: Record<string, AnchorPose>;
        bindings?: Record<string, BindingPose>;
      };
      boneOverridesByPose = parsed.poses || (parsed.bones ? { [parsed.poseId || selectedPoseId]: parsed.bones } : {});
      anchorOverridesByPose = parsed.anchorPoses || {};
      bindingOverridesByPose = parsed.bindingPoses || {};
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

  async function loadRigActions() {
    try {
      const response = await fetch("/__rig/actions");
      if (!response.ok) throw new Error(await response.text());
      rigActionManifest = (await response.json()) as RigActionManifest;
      if (!rigActionManifest.actions.some((action) => action.id === selectedRigActionId)) {
        selectedRigActionId = rigActionManifest.actions[0]?.id || "";
      }
      rigActionStatus = "Loaded actions";
    } catch (error) {
      rigActionStatus = error instanceof Error ? `Load failed: ${error.message}` : "Load failed";
    }
  }

  async function saveRigActions() {
    try {
      persistToolSelection();
      const response = await fetch("/__rig/actions", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(rigActionManifest)
      });
      if (!response.ok) throw new Error(await response.text());
      rigActionStatus = "Saved actions";
    } catch (error) {
      rigActionStatus = error instanceof Error ? `Save failed: ${error.message}` : "Save failed";
    }
  }

  function updateRigAction(patch: Partial<BattleActionDefinition>) {
    if (!selectedRigAction) return;
    rigActionManifest = {
      ...rigActionManifest,
      actions: rigActionManifest.actions.map((action) => (action.id === selectedRigAction.id ? { ...action, ...patch } : action))
    };
  }

  function renameRigAction(id: string) {
    if (!selectedRigAction) return;
    const clean = id.trim();
    if (!clean || rigActionManifest.actions.some((action) => action.id === clean && action.id !== selectedRigAction.id)) {
      rigActionStatus = "Action id must be unique";
      return;
    }
    rigActionManifest = {
      ...rigActionManifest,
      actions: rigActionManifest.actions.map((action) => (action.id === selectedRigAction.id ? { ...action, id: clean } : action))
    };
    selectedRigActionId = clean;
  }

  function renameSelectedPoseId(id: string) {
    const fromId = selectedPoseId;
    const clean = cleanPoseId(id);
    if (!fromId || !clean) {
      saveStatus = "Pose id is required";
      return;
    }
    if (clean === fromId) return;

    const library = currentProjectPoseLibrary();
    if (library[clean]) {
      saveStatus = "Pose id must be unique";
      return;
    }

    const source = library[fromId] || poseDefinitionById(fromId) || selectedPose;
    if (!source) {
      saveStatus = "Pose not found";
      return;
    }

    const { [fromId]: _removed, ...rest } = library;
    projectPoseLibrary = {
      ...rest,
      [clean]: {
        ...cloneRecord(source),
        id: clean
      }
    };
    rigActionManifest = {
      ...rigActionManifest,
      actions: rigActionManifest.actions.map((action) => ({
        ...action,
        poseId: action.poseId === fromId ? clean : action.poseId,
        sequence: action.sequence.map((poseId) => (poseId === fromId ? clean : poseId))
      }))
    };
    boneOverridesByPose = moveRecordKey(boneOverridesByPose, fromId, clean);
    anchorOverridesByPose = moveRecordKey(anchorOverridesByPose, fromId, clean);
    selectedPoseId = clean;
    poseIdDraft = clean;
    lastPoseIdDraftSource = clean;
    saveStatus = "Renamed pose";
    rigActionStatus = "Updated pose references";
  }

  function updateSelectedPoseMetadata(patch: Partial<SkeletalPoseDefinition>, status = "Updated pose") {
    if (!selectedPoseId) return;
    const source = poseDefinitionById(selectedPoseId) || selectedPose || { id: selectedPoseId, name: selectedPoseId, bones: {}, anchors: {} };
    projectPoseLibrary = {
      ...currentProjectPoseLibrary(),
      [selectedPoseId]: {
        ...cloneRecord(source),
        ...patch,
        id: selectedPoseId
      }
    };
    saveStatus = status;
  }

  function updateSelectedPoseDuration(durationMs: number) {
    if (!Number.isFinite(durationMs)) return;
    updateSelectedPoseMetadata({ durationMs: Math.max(40, Math.round(durationMs)) }, "Updated pose duration");
  }

  function inputValueFromForm(event: SubmitEvent, name: string) {
    const form = event.currentTarget as HTMLFormElement;
    const input = form.elements.namedItem(name) as HTMLInputElement | null;
    return input?.value || "";
  }

  function submitSelectedPoseId(event: SubmitEvent) {
    event.preventDefault();
    renameSelectedPoseId(inputValueFromForm(event, "poseId") || poseIdDraft);
  }

  function submitSelectedPoseName(event: SubmitEvent) {
    event.preventDefault();
    const name = (inputValueFromForm(event, "poseName") || poseNameDraft).trim();
    updateSelectedPoseMetadata({ name: name || selectedPoseId }, "Updated pose name");
  }

  function submitSelectedPoseDuration(event: SubmitEvent) {
    event.preventDefault();
    updateSelectedPoseDuration(Number(inputValueFromForm(event, "poseDuration") || poseDurationDraft));
  }

  function createRigAction() {
    const id = uniqueRigActionId("rig.custom.action");
    const poseId = createPoseDraftFrom(selectedPoseId, `${id}_pose`, "Custom action pose");
    const action: BattleActionDefinition = {
      id,
      label: "Custom action",
      rig: "segmented-v12",
      style: "fist",
      poseId,
      sequence: [poseId],
      tags: ["custom"],
      durationMs: 720,
      actorMotion: "approach",
      targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
      vfx: []
    };
    rigActionManifest = { ...rigActionManifest, actions: [...rigActionManifest.actions, action] };
    selectedRigActionId = id;
    selectedPoseId = poseId;
    activeInspectorTab = "pose";
  }

  function duplicateRigAction() {
    if (!selectedRigAction) return;
    const id = uniqueRigActionId(`${selectedRigAction.id}.copy`);
    const poseMap = duplicateActionPoseSet(selectedRigAction, id);
    const nextPoseId = poseMap.get(selectedRigAction.poseId) || selectedRigAction.poseId;
    const nextSequence = selectedRigAction.sequence.map((poseId) => poseMap.get(poseId) || poseId);
    rigActionManifest = {
      ...rigActionManifest,
      actions: [
        ...rigActionManifest.actions,
        {
          ...JSON.parse(JSON.stringify(selectedRigAction)),
          id,
          label: `${selectedRigAction.label} copy`,
          poseId: nextPoseId,
          sequence: nextSequence.length ? nextSequence : [nextPoseId]
        }
      ]
    };
    selectedRigActionId = id;
    selectedPoseId = nextPoseId;
    activeInspectorTab = "pose";
  }

  function deleteRigAction() {
    if (!selectedRigAction || rigActionManifest.actions.length <= 1) return;
    const nextActions = rigActionManifest.actions.filter((action) => action.id !== selectedRigAction.id);
    rigActionManifest = { ...rigActionManifest, actions: nextActions };
    selectedRigActionId = nextActions[0]?.id || "";
  }

  function uniqueRigActionId(base: string) {
    const used = new Set(rigActionManifest.actions.map((action) => action.id));
    if (!used.has(base)) return base;
    let index = 2;
    while (used.has(`${base}_${index}`)) index += 1;
    return `${base}_${index}`;
  }

  function createPoseDraftFrom(sourcePoseId: string, baseId: string, name: string) {
    const source = poseDefinitionById(sourcePoseId) || selectedPose || Object.values(rig?.poses || {})[0];
    const id = uniquePoseId(sanitizePoseId(baseId));
    const pose = clonePoseAs(source, id, name || id);
    projectPoseLibrary = { ...currentProjectPoseLibrary(), [id]: pose };
    saveStatus = "Created pose";
    return id;
  }

  function duplicateActionPoseSet(action: BattleActionDefinition, targetActionId: string) {
    const poseMap = new Map<string, string>();
    let nextLibrary = currentProjectPoseLibrary();
    for (const sourcePoseId of uniqueTextList([action.poseId, ...action.sequence])) {
      const source = nextLibrary[sourcePoseId] || poseDefinitionById(sourcePoseId);
      if (!source) continue;
      const id = uniquePoseId(`${sanitizePoseId(targetActionId)}_${sanitizePoseId(sourcePoseId)}`, nextLibrary);
      poseMap.set(sourcePoseId, id);
      nextLibrary = {
        ...nextLibrary,
        [id]: clonePoseAs(source, id, `${action.label} ${source.name}`)
      };
    }
    projectPoseLibrary = nextLibrary;
    saveStatus = "Copied poses";
    return poseMap;
  }

  function copySelectedPoseToCurrentAction() {
    if (!selectedRigAction || !selectedPose) return;
    const id = createPoseDraftFrom(selectedPoseId, `${selectedRigAction.id}_${selectedPoseId}`, `${selectedRigAction.label} ${selectedPose.name}`);
    attachPoseToCurrentAction(id, selectedPoseId);
    selectPose(id);
    activeLibraryTab = "poses";
  }

  function createBlankPoseForCurrentAction() {
    if (!selectedRigAction) return;
    const id = uniquePoseId(`${sanitizePoseId(selectedRigAction.id)}_blank`);
    projectPoseLibrary = {
      ...currentProjectPoseLibrary(),
      [id]: { id, name: `${selectedRigAction.label} pose`, durationMs: 240, bones: {}, anchors: {} }
    };
    attachPoseToCurrentAction(id, selectedPoseId);
    selectPose(id);
    activeLibraryTab = "poses";
    saveStatus = "Created blank pose";
  }

  function attachPoseToCurrentAction(poseId: string, sourcePoseId: string) {
    if (!selectedRigAction) return;
    const sequence = selectedActionSequence.length ? selectedActionSequence : [selectedRigAction.poseId];
    let nextSequence = sequence.map((candidate) => (candidate === sourcePoseId ? poseId : candidate));
    if (!nextSequence.includes(poseId)) nextSequence = [...nextSequence, poseId];
    updateRigAction({
      poseId: selectedRigAction.poseId === sourcePoseId ? poseId : selectedRigAction.poseId,
      sequence: nextSequence
    });
  }

  function currentProjectPoseLibrary() {
    return clonePoseLibrary(projectPoseLibrary || rig?.poses || {});
  }

  function poseDefinitionById(poseId: string) {
    return projectPoseLibrary?.[poseId] || rig?.poses[poseId] || null;
  }

  function clonePoseAs(source: SkeletalPoseDefinition | null | undefined, id: string, name: string): SkeletalPoseDefinition {
    const sourcePose = source || { id, name, bones: {}, anchors: {} };
    const bones = { ...(sourcePose.bones || {}), ...(boneOverridesByPose[sourcePose.id] || {}) };
    const anchors = { ...(sourcePose.anchors || {}), ...(anchorOverridesByPose[sourcePose.id] || {}) };
    const bindingOverrides = bindingOverridesByPose[sourcePose.id] || {};
    const bindings = sourcePose.bindings || Object.keys(bindingOverrides).length ? { ...(sourcePose.bindings || {}), ...bindingOverrides } : undefined;
    return {
      ...JSON.parse(JSON.stringify(sourcePose)),
      id,
      name,
      bones: cloneRecord(bones),
      anchors: cloneRecord(anchors),
      ...(bindings ? { bindings } : {})
    };
  }

  function cloneRecord<T>(value: T): T {
    return JSON.parse(JSON.stringify(value)) as T;
  }

  function compactBindingPose(value: BindingPose): BindingPose {
    return Object.fromEntries(Object.entries(value).filter(([, entryValue]) => entryValue !== undefined && entryValue !== "")) as BindingPose;
  }

  function moveRecordKey<T>(record: Record<string, T>, fromId: string, toId: string) {
    if (!(fromId in record)) return record;
    const { [fromId]: value, ...rest } = record;
    return { ...rest, [toId]: value };
  }

  function uniquePoseId(base: string, library: Record<string, SkeletalPoseDefinition> = currentProjectPoseLibrary()) {
    const clean = sanitizePoseId(base) || "pose";
    const used = new Set(Object.keys(library));
    if (!used.has(clean)) return clean;
    let index = 2;
    while (used.has(`${clean}_${index}`)) index += 1;
    return `${clean}_${index}`;
  }

  function sanitizePoseId(value: string) {
    return value.replace(/^rig[._-]*/i, "").replace(/[^a-zA-Z0-9]+/g, "_").replace(/^_+|_+$/g, "") || "pose";
  }

  function cleanPoseId(value: string) {
    return value.trim().replace(/[^a-zA-Z0-9_-]+/g, "_").replace(/^_+|_+$/g, "");
  }

  async function loadMartialArts() {
    try {
      const response = await fetch("/__rig/martial-arts");
      if (!response.ok) throw new Error(await response.text());
      const payload = (await response.json()) as { files: MartialArtFile[] };
      martialFiles = payload.files || [];
      selectedMartialFile = martialFiles[0]?.file || "";
      const parsed = parseMartialFile(martialFiles[0]?.text || "");
      selectedMartialArtKey = parsed.arts[0]?.key || "";
      selectedMappingTargetKey = parsed.arts[0] ? buildMartialTargets(parsed.arts[0].data)[0]?.key || "" : "";
      selectedMappingPoolId = parsed.arts[0] ? Object.keys(parsed.arts[0].data.animation_pools || {})[0] || "basic" : "basic";
      martialStatus = "Loaded martial arts";
    } catch (error) {
      martialStatus = error instanceof Error ? `Load failed: ${error.message}` : "Load failed";
    }
  }

  async function saveSelectedMartialArt() {
    if (!selectedMartial) return;
    try {
      const response = await fetch("/__rig/martial-arts", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(selectedMartial)
      });
      if (!response.ok) throw new Error(await response.text());
      martialStatus = "Saved martial art";
    } catch (error) {
      martialStatus = error instanceof Error ? `Save failed: ${error.message}` : "Save failed";
    }
  }

  function updateSelectedMartialText(text: string) {
    if (!selectedMartial) return;
    martialFiles = martialFiles.map((file) => (file.file === selectedMartial.file ? { ...file, text } : file));
  }

  function copySelectedRigActionId() {
    if (!selectedRigAction) return;
    void navigator.clipboard?.writeText(selectedRigAction.id);
    rigActionStatus = "Copied action id";
  }

  function commaList(value: string) {
    return value
      .split(",")
      .map((item) => item.trim())
      .filter(Boolean);
  }

  function selectMartialFile(file: string) {
    selectedMartialFile = file;
    const parsed = parseMartialFile(martialFiles.find((item) => item.file === file)?.text || "");
    selectedMartialArtKey = parsed.arts[0]?.key || "";
    selectedMappingTargetKey = parsed.arts[0] ? buildMartialTargets(parsed.arts[0].data)[0]?.key || "" : "";
    selectedMappingPoolId = parsed.arts[0] ? Object.keys(parsed.arts[0].data.animation_pools || {})[0] || "basic" : "basic";
  }

  function selectMartialArt(key: string) {
    selectedMartialArtKey = key;
    const art = martialParse.arts.find((item) => item.key === key);
    selectedMappingTargetKey = art ? buildMartialTargets(art.data)[0]?.key || "" : "";
    selectedMappingPoolId = art ? Object.keys(art.data.animation_pools || {})[0] || "basic" : "basic";
  }

  function parseMartialFile(text: string): MartialParseResult {
    if (!text.trim()) return { root: null, arts: [], error: "" };
    try {
      const root = parseYaml(text) as unknown;
      const docs = Array.isArray(root) ? root : root && typeof root === "object" ? [root] : [];
      const arts = docs
        .filter((item): item is YamlObject => !!item && typeof item === "object" && !Array.isArray(item))
        .map((data, index) => ({
          key: `${index}:${String(data.id || index)}`,
          index,
          data,
          id: String(data.id || `art_${index}`),
          name: String(data.name || data.id || `art ${index + 1}`)
        }));
      return { root, arts, error: "" };
    } catch (error) {
      return { root: null, arts: [], error: error instanceof Error ? error.message : "Invalid YAML" };
    }
  }

  function buildMartialTargets(art: YamlObject): MartialTarget[] {
    return (["attack_moves", "active_skills"] as const).flatMap((kind) =>
      ((Array.isArray(art[kind]) ? art[kind] : []) as unknown[])
        .filter((item): item is YamlObject => !!item && typeof item === "object" && !Array.isArray(item))
        .map((data, index) => ({
          key: `${kind}:${index}:${String(data.id || index)}`,
          kind,
          index,
          data,
          id: String(data.id || `${kind}_${index}`),
          name: String(data.name || data.id || `${kind} ${index + 1}`)
        }))
    );
  }

  function actionIdsForStyle(style: CombatStyle | "all" = "all") {
    return rigActionManifest.actions.filter((action) => style === "all" || action.style === style || action.id.startsWith("rig.effect.")).map((action) => action.id);
  }

  function styleFilterForArt(value: unknown): StyleFilter {
    return value === "sword" || value === "fist" ? value : "all";
  }

  function poolIds(art: YamlObject) {
    return Object.keys((art.animation_pools || {}) as Record<string, unknown>);
  }

  function resolvedPoolId(art: YamlObject) {
    const ids = poolIds(art);
    if (ids.includes(selectedMappingPoolId)) return selectedMappingPoolId;
    return ids[0] || "";
  }

  function poolEntries(art: YamlObject, poolId: string) {
    const pool = (art.animation_pools || {})[poolId];
    return Array.isArray(pool?.actions) ? (pool.actions as YamlObject[]) : [];
  }

  function selectedTargetAnimation() {
    return ((currentMappingTarget?.data.animation || {}) as YamlObject) || {};
  }

  function editMartialRoot(mutator: (root: unknown, art: YamlObject) => void) {
    if (!selectedMartial || !currentMartialArt) return;
    const parsed = parseMartialFile(selectedMartial.text);
    if (parsed.error) {
      martialStatus = `Parse failed: ${parsed.error}`;
      return;
    }
    const root = parsed.root;
    const docs = Array.isArray(root) ? root : root && typeof root === "object" ? [root] : [];
    const art = docs[currentMartialArt.index];
    if (!art || typeof art !== "object" || Array.isArray(art)) return;
    mutator(root, art as YamlObject);
    updateSelectedMartialText(`${stringifyYaml(root, { lineWidth: 0 })}`);
  }

  function updateTargetAnimation(patch: YamlObject) {
    if (!currentMappingTarget) return;
    if (typeof patch.pool === "string") selectedMappingPoolId = patch.pool;
    editMartialRoot((_root, art) => {
      const targets = Array.isArray(art[currentMappingTarget.kind]) ? (art[currentMappingTarget.kind] as YamlObject[]) : [];
      const target = targets[currentMappingTarget.index];
      if (!target) return;
      const next = { ...((target.animation || {}) as YamlObject), ...patch };
      if ("action" in patch) delete next.pool;
      if ("pool" in patch) delete next.action;
      target.animation = next;
    });
  }

  function setTargetAnimationMode(mode: "pool" | "action") {
    if (!currentMartialArt || !currentMappingTarget) return;
    if (mode === "pool") {
      updateTargetAnimation({ pool: poolIds(currentMartialArt.data)[0] || "basic" });
    } else {
      updateTargetAnimation({ action: actionIdsForStyle(styleFilterForArt(currentMartialArt.data.type))[0] || rigActionManifest.actions[0]?.id || "" });
    }
  }

  function updateTargetTags(value: string) {
    updateTargetAnimation({ tags: commaList(value) });
  }

  function updatePoolEntry(index: number, patch: YamlObject) {
    if (!currentMartialArt || !currentPoolId) return;
    const normalizedPatch = { ...patch };
    if ("weight" in normalizedPatch) normalizedPatch.weight = Math.max(1, Number(normalizedPatch.weight) || 1);
    editMartialRoot((_root, art) => {
      art.animation_pools = art.animation_pools || {};
      art.animation_pools[currentPoolId] = art.animation_pools[currentPoolId] || { actions: [] };
      const entries = (art.animation_pools[currentPoolId].actions = Array.isArray(art.animation_pools[currentPoolId].actions) ? art.animation_pools[currentPoolId].actions : []);
      entries[index] = { ...(entries[index] || {}), ...normalizedPatch };
    });
  }

  function addPoolEntry() {
    if (!currentMartialArt) return;
    const poolId = currentPoolId || selectedMappingPoolId || "basic";
    editMartialRoot((_root, art) => {
      art.animation_pools = art.animation_pools || {};
      art.animation_pools[poolId] = art.animation_pools[poolId] || { actions: [] };
      const entries = (art.animation_pools[poolId].actions = Array.isArray(art.animation_pools[poolId].actions) ? art.animation_pools[poolId].actions : []);
      entries.push({ action: actionIdsForStyle(styleFilterForArt(art.type))[0] || rigActionManifest.actions[0]?.id || "", weight: 1, tags: [] });
    });
    selectedMappingPoolId = poolId;
  }

  function removePoolEntry(index: number) {
    if (!currentMartialArt || !currentPoolId) return;
    if (currentPoolEntries.length <= 1) {
      martialStatus = "Pool must keep at least one action";
      return;
    }
    editMartialRoot((_root, art) => {
      const entries = art.animation_pools?.[currentPoolId]?.actions;
      if (Array.isArray(entries)) entries.splice(index, 1);
    });
  }

  function addAnimationPool() {
    if (!currentMartialArt) return;
    const id = uniquePoolId(currentMartialArt.data, "new_pool");
    editMartialRoot((_root, art) => {
      art.animation_pools = art.animation_pools || {};
      art.animation_pools[id] = { actions: [{ action: actionIdsForStyle(styleFilterForArt(art.type))[0] || rigActionManifest.actions[0]?.id || "", weight: 1, tags: [] }] };
    });
    selectedMappingPoolId = id;
  }

  function renameAnimationPool(nextId: string) {
    if (!currentMartialArt || !currentPoolId) return;
    const fromId = currentPoolId;
    const clean = nextId.trim();
    if (!clean) {
      martialStatus = "Pool id is required";
      return;
    }
    if (clean === fromId) return;
    if (poolIds(currentMartialArt.data).includes(clean)) {
      martialStatus = "Pool id must be unique";
      return;
    }
    editMartialRoot((_root, art) => {
      art.animation_pools = art.animation_pools || {};
      const pools = art.animation_pools as Record<string, YamlObject>;
      const pool = pools[fromId] || { actions: [] };
      delete pools[fromId];
      pools[clean] = pool;
      rewriteAnimationPoolRefs(art, fromId, clean);
    });
    selectedMappingPoolId = clean;
    martialStatus = "Renamed pool";
  }

  function deleteAnimationPool() {
    if (!currentMartialArt || !currentPoolId) return;
    const fromId = currentPoolId;
    const remainingIds = poolIds(currentMartialArt.data).filter((id) => id !== fromId);
    const fallbackId = remainingIds[0] || "basic";
    editMartialRoot((_root, art) => {
      art.animation_pools = art.animation_pools || {};
      const pools = art.animation_pools as Record<string, YamlObject>;
      delete pools[fromId];
      if (remainingIds.length === 0) {
        pools[fallbackId] = {
          actions: [{ action: actionIdsForStyle(styleFilterForArt(art.type))[0] || rigActionManifest.actions[0]?.id || "", weight: 1, tags: [] }]
        };
      }
      rewriteAnimationPoolRefs(art, fromId, fallbackId);
    });
    selectedMappingPoolId = fallbackId;
    martialStatus = "Deleted pool";
  }

  function rewriteAnimationPoolRefs(art: YamlObject, fromId: string, toId: string) {
    for (const kind of ["attack_moves", "active_skills"] as const) {
      const targets = Array.isArray(art[kind]) ? (art[kind] as YamlObject[]) : [];
      for (const target of targets) {
        const animation = target.animation as YamlObject | undefined;
        if (animation?.pool === fromId) animation.pool = toId;
      }
    }
  }

  function uniquePoolId(art: YamlObject, base: string) {
    const used = new Set(poolIds(art));
    if (!used.has(base)) return base;
    let index = 2;
    while (used.has(`${base}_${index}`)) index += 1;
    return `${base}_${index}`;
  }

  function updateTargetReaction(result: CombatResult, reaction: TargetReaction) {
    if (!selectedRigAction) return;
    updateRigAction({
      targetReaction: {
        ...selectedRigAction.targetReaction,
        [result]: reaction
      }
    });
  }

  function addVfx() {
    if (!selectedRigAction) return;
    updateRigAction({
      vfx: [...selectedRigAction.vfx, { kind: "impact", variant: "hit-spark", anchor: "target" }]
    });
  }

  function updateVfx(index: number, patch: Partial<BattleActionDefinition["vfx"][number]>) {
    if (!selectedRigAction) return;
    updateRigAction({
      vfx: selectedRigAction.vfx.map((vfx, vfxIndex) => (vfxIndex === index ? { ...vfx, ...patch } : vfx))
    });
  }

  function removeVfx(index: number) {
    if (!selectedRigAction) return;
    updateRigAction({
      vfx: selectedRigAction.vfx.filter((_, vfxIndex) => vfxIndex !== index)
    });
  }

  function storageKey(entryId: string) {
    return `wuxia-mud.animation-rig.${entryId}.v3`;
  }

  function mergedProjectPoseLibrary(rigValue: SkeletonRigDefinition): Record<string, SkeletalPoseDefinition> {
    const poses = clonePoseLibrary(projectPoseLibrary || rigValue.poses);
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
    for (const [poseId, bindings] of Object.entries(bindingOverridesByPose)) {
      if (!poses[poseId]) continue;
      poses[poseId] = {
        ...poses[poseId],
        bindings: {
          ...(poses[poseId].bindings || {}),
          ...bindings
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
        bindingPoses: bindingOverridesByPose,
        poseBindings: currentPoseBindingOverrides,
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

  function playbackPose(rigValue: SkeletonRigDefinition, timeMs: number, selectedId: string, actionSequenceIds: string[] = []) {
    const poseIds = actionSequenceIds.filter((id) => rigValue.poses[id]);
    const fallbackPoseIds = poseIds.length >= 2 ? poseIds : playbackPoseIds(rigValue, selectedId);
    if (fallbackPoseIds.length < 2) return rigValue.poses[selectedId] || rigValue.poses.idle || Object.values(rigValue.poses)[0];
    const segments = fallbackPoseIds.slice(1).map((id, index) => {
      const from = rigValue.poses[fallbackPoseIds[index]];
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
      <p>{rigActionManifest.actions.length} actions / {actionPoseOptions.length} poses / {entries.length} rig previews</p>
    </div>
    <div class="rig-header-meta">
      <span>{selectedRigAction?.id || "-"}</span>
      <span>{selectedRigAction?.style || "-"}</span>
      <span>{selectedEntry ? `${selectedEntry.profile} preview` : "no rig preview"}</span>
    </div>
  </header>

  <nav class="rig-top-tabs" aria-label="Tool views">
    <button type="button" class:active={activeTab === "actions"} on:click={() => (activeTab = "actions")}>动作库</button>
    <button type="button" class:active={activeTab === "mapping"} on:click={() => (activeTab = "mapping")}>武功映射</button>
  </nav>

  {#if activeTab === "actions"}
  <main class="rig-tool-layout">
    <aside class="rig-list-panel">
      <div class="rig-panel-title">
        <h2>{activeLibraryTab === "actions" ? "动作定义" : "姿势库"}</h2>
        <span>
          {activeLibraryTab === "actions"
            ? `${filteredRigActions.length} / ${rigActionManifest.actions.length}`
            : `${filteredPoseOptions.length} / ${actionPoseOptions.length}`}
        </span>
      </div>

      <div class="rig-library-tabs" aria-label="Rig library">
        <button type="button" class:active={activeLibraryTab === "actions"} on:click={() => (activeLibraryTab = "actions")}>动作</button>
        <button type="button" class:active={activeLibraryTab === "poses"} on:click={() => (activeLibraryTab = "poses")}>姿势</button>
      </div>

      <div class="rig-filter-row">
        <input
          aria-label={activeLibraryTab === "actions" ? "Search actions" : "Search poses"}
          placeholder={activeLibraryTab === "actions" ? "搜索动作 ID / 标签" : "搜索姿势 ID / 名称 / 引用动作"}
          bind:value={search}
        />
      </div>

      {#if activeLibraryTab === "actions"}
        <div class="rig-filter-grid">
          <select bind:value={styleFilter} aria-label="Style filter">
            <option value="all">全部流派</option>
            <option value="sword">剑</option>
            <option value="fist">拳</option>
          </select>
        </div>
      {:else}
        <div class="rig-library-summary">
          选择姿势会切到右侧“姿势”面板；改姿势 ID 会同步更新所有动作引用。
        </div>
      {/if}

      <div class="rig-entry-list" aria-label={activeLibraryTab === "actions" ? "Rig action definitions" : "Rig pose definitions"}>
        {#if activeLibraryTab === "actions"}
          {#each filteredRigActions as action (action.id)}
            <button type="button" class:active={action.id === selectedRigAction?.id} on:click={() => selectRigAction(action.id)}>
              <strong>{action.id}</strong>
              <span>{action.label}</span>
              <small>{action.style} · {entryCountForAction(action.id)} previews · {action.durationMs}ms</small>
            </button>
          {/each}
        {:else}
          {#each filteredPoseOptions as pose (pose.id)}
            <button type="button" class:active={pose.id === selectedPoseId} on:click={() => selectPoseFromLibrary(pose.id)}>
              <strong>{pose.name}</strong>
              <span>{pose.id}</span>
              <small>{poseUsageCount(pose.id)} actions · {pose.durationMs || 0}ms · {poseUsageSummary(pose.id)}</small>
            </button>
          {:else}
            <div class="rig-empty-list">没有匹配的姿势</div>
          {/each}
        {/if}
      </div>
    </aside>

    <section class="rig-workbench">
      <div class="rig-preview-head">
        <div>
          <strong>{selectedRigAction?.label || "No action"}</strong>
          <span>{selectedRigAction?.id || "-"}</span>
        </div>
        <label>
          预览 rig
          <select value={selectedEntry?.id || ""} disabled={selectedActionEntries.length === 0} aria-label="预览 rig 条目" on:change={(event) => selectEntry(selectInput(event))}>
            {#if selectedActionEntries.length === 0}
              <option value="">无可预览 rig</option>
            {:else}
              {#each selectedActionEntries as entry (entry.id)}
                <option value={entry.id}>{previewEntryLabel(entry)}</option>
              {/each}
            {/if}
          </select>
        </label>
      </div>

      <div class="rig-toolbar">
        <div class="rig-segmented edit-modes">
          <button
            type="button"
            class:active={dragMode === "pose"}
            aria-pressed={dragMode === "pose"}
            title={dragModeTitles.pose}
            on:click={() => (dragMode = "pose")}
          >
            骨骼
          </button>
          <button
            type="button"
            class:active={dragMode === "anchor"}
            aria-pressed={dragMode === "anchor"}
            title={dragModeTitles.anchor}
            on:click={() => (dragMode = "anchor")}
          >
            锚点
          </button>
          <button
            type="button"
            class:active={dragMode === "binding"}
            aria-pressed={dragMode === "binding"}
            title={dragModeTitles.binding}
            on:click={() => (dragMode = "binding")}
          >
            贴图
          </button>
        </div>
        <div class="rig-segmented playback-controls">
          <button type="button" on:click={() => stepPose(-1)}>上一姿势</button>
          <button type="button" on:click={() => stepPose(1)}>下一姿势</button>
          <button type="button" class:active={isPlaying} on:click={togglePlayback}>{isPlaying ? "暂停" : "播放预览"}</button>
          <button type="button" on:click={resetPlayback}>重播</button>
        </div>

        <label><input type="checkbox" bind:checked={showImages} /> 原图</label>
        <label><input type="checkbox" bind:checked={showSkin} /> 蒙皮</label>
        <label><input type="checkbox" bind:checked={showBones} /> 骨架</label>
        <label><input type="checkbox" bind:checked={showAnchors} /> 锚点</label>
        <label><input type="checkbox" bind:checked={showBindings} /> 绑定</label>
        <label><input type="checkbox" bind:checked={showLabels} /> 标签</label>
        <label><input type="checkbox" bind:checked={lockLimbLengths} /> 锁骨长</label>
        <label class="rig-zoom">缩放 <input type="range" min="0.45" max="7" step="0.05" value={zoom} on:input={handleZoomInput} /></label>
        <button type="button" class="rig-secondary" on:click={resetView}>视图复位</button>
      </div>

      {#if rig}
        <canvas
          class="rig-canvas"
          bind:this={canvasEl}
          on:wheel|nonpassive={handleCanvasWheel}
          on:pointerdown={handlePointerDown}
          on:pointermove={handlePointerMove}
          on:pointerup={handlePointerUp}
          on:pointercancel={handlePointerUp}
        ></canvas>
      {:else}
        <div class="rig-empty-state">
          <strong>No rig preview</strong>
          <span>{selectedRigAction?.id || "-"} has no matching skeletal entry yet.</span>
        </div>
      {/if}

      <div class="rig-status-row">
        <span>编辑: {dragModeLabels[dragMode]}</span>
        <span>锚点: {selectedAnchor?.id || "-"}</span>
        <span>骨骼: {selectedBone?.id || "-"}</span>
        <span>绑定: {selectedBinding?.id || "-"}</span>
        <span>视图: {zoom.toFixed(2)}x / {Math.round(viewportPan.x)}, {Math.round(viewportPan.y)}</span>
      </div>
    </section>

    <aside class="rig-inspector">
      <nav class="rig-inspector-tabs" aria-label="Action inspector panels">
        <button type="button" class:active={activeInspectorTab === "action"} on:click={() => (activeInspectorTab = "action")}>属性</button>
        <button type="button" class:active={activeInspectorTab === "feedback"} on:click={() => (activeInspectorTab = "feedback")}>反应</button>
        <button type="button" class:active={activeInspectorTab === "pose"} on:click={() => (activeInspectorTab = "pose")}>姿势</button>
        <button type="button" class:active={activeInspectorTab === "anchor"} on:click={() => (activeInspectorTab = "anchor")}>锚点</button>
        <button type="button" class:active={activeInspectorTab === "binding"} on:click={() => (activeInspectorTab = "binding")}>贴图</button>
        <button type="button" class:active={activeInspectorTab === "map"} on:click={() => (activeInspectorTab = "map")}>Map</button>
        <button type="button" class:active={activeInspectorTab === "io"} on:click={() => (activeInspectorTab = "io")}>JSON</button>
      </nav>
      {#if activeInspectorTab === "action"}
      <section>
        <div class="rig-section-head">
          <h2>动作属性</h2>
          <span>{rigActionStatus}</span>
        </div>

        {#if selectedRigAction}
          <div class="rig-action-card">
            <span class="rig-action-eyebrow">当前动作</span>
            <strong>{selectedRigAction.label}</strong>
            <code>{selectedRigAction.id}</code>
          </div>

          <div class="rig-action-meta-grid">
            <div>
              <span>风格</span>
              <strong>{selectedRigAction.style}</strong>
            </div>
            <div>
              <span>位移</span>
              <strong>{selectedRigAction.actorMotion}</strong>
            </div>
            <div>
              <span>主姿势</span>
              <strong class:missing={rig && !rig.poses[selectedRigAction.poseId]}>{poseLabelById(selectedRigAction.poseId, rig)}</strong>
            </div>
            <div>
              <span>时长</span>
              <strong>{selectedRigAction.durationMs}ms</strong>
            </div>
          </div>

          <div class="rig-chip-block">
            <span>播放序列</span>
            <div class="rig-chip-list">
              {#each selectedActionSequence as poseId}
                <code class:missing={rig && !rig.poses[poseId]}>{poseId}</code>
              {/each}
            </div>
            {#if missingActionPoseIds.length > 0}
              <div class="rig-warning-line">缺失姿势：{missingActionPoseIds.join(", ")}</div>
            {/if}
          </div>

          <div class="rig-chip-block">
            <span>标签</span>
            <div class="rig-chip-list">
              {#each selectedRigAction.tags as tag}
                <code>{tag}</code>
              {/each}
            </div>
          </div>

          <div class="rig-action-row compact-two">
            <button type="button" class="rig-secondary" on:click={copySelectedRigActionId}>复制 ID</button>
            <button type="button" class="rig-secondary" on:click={duplicateRigAction}>复制动作+姿势</button>
          </div>
          <button type="button" class="rig-primary" on:click={saveRigActions}>保存动作库</button>

          <details class="rig-advanced-editor">
            <summary>编辑字段</summary>
            <div class="rig-advanced-editor-body">
              <label class="rig-field">
                ID
                <input value={selectedRigAction.id} on:change={(event) => renameRigAction((event.currentTarget as HTMLInputElement).value)} />
              </label>
              <label class="rig-field">
                Label
                <input value={selectedRigAction.label} on:input={(event) => updateRigAction({ label: (event.currentTarget as HTMLInputElement).value })} />
              </label>
              <div class="rig-pair-fields">
                <label>
                  Style
                  <select value={selectedRigAction.style} on:change={(event) => updateRigAction({ style: selectInput(event) as CombatStyle })}>
                    <option value="sword">sword</option>
                    <option value="fist">fist</option>
                  </select>
                </label>
                <label>
                  Motion
                  <select value={selectedRigAction.actorMotion} on:change={(event) => updateRigAction({ actorMotion: selectInput(event) as ActorMotion })}>
                    <option value="none">none</option>
                    <option value="approach">approach</option>
                    <option value="lunge">lunge</option>
                    <option value="drive">drive</option>
                    <option value="focus">focus</option>
                  </select>
                </label>
              </div>
              <div class="rig-pair-fields">
                <label>
                  主姿势
                  <select value={selectedRigAction.poseId} disabled={!rig} on:change={(event) => setPrimaryPoseId(selectInput(event))}>
                    {#if rig && !rig.poses[selectedRigAction.poseId]}
                      <option value={selectedRigAction.poseId}>缺失姿势 / {selectedRigAction.poseId}</option>
                    {/if}
                    {#each actionPoseOptions as pose (pose.id)}
                      <option value={pose.id}>{pose.name} / {pose.id}</option>
                    {/each}
                  </select>
                </label>
                <label>
                  时长
                  <input type="number" min="80" step="20" value={selectedRigAction.durationMs} on:input={(event) => updateRigAction({ durationMs: numericInput(event) })} />
                </label>
              </div>
              <div class="rig-sequence-editor">
                <div class="rig-sequence-editor-head">
                  <span>播放序列</span>
                  <button type="button" class="rig-secondary" on:click={() => addSequencePose(selectedActionSequence.length - 1)}>添加姿势</button>
                </div>
                <div class="rig-sequence-list">
                  {#each selectedActionSequence as poseId, index (`${index}-${poseId}`)}
                    <div class="rig-sequence-row" class:missing={rig && !rig.poses[poseId]}>
                      <span class="rig-sequence-index">{index + 1}</span>
                      <select value={poseId} disabled={!rig} on:change={(event) => updateSequencePose(index, selectInput(event))}>
                        {#if rig && !rig.poses[poseId]}
                          <option value={poseId}>缺失姿势 / {poseId}</option>
                        {/if}
                        {#each actionPoseOptions as pose (pose.id)}
                          <option value={pose.id}>{pose.name} / {pose.id}</option>
                        {/each}
                      </select>
                      <div class="rig-sequence-actions">
                        <button type="button" class="rig-secondary" disabled={index === 0} on:click={() => moveSequencePose(index, -1)}>上移</button>
                        <button type="button" class="rig-secondary" disabled={index === selectedActionSequence.length - 1} on:click={() => moveSequencePose(index, 1)}>下移</button>
                        <button type="button" class="rig-secondary" on:click={() => addSequencePose(index)}>插入</button>
                        <button type="button" class="rig-secondary danger" disabled={selectedActionSequence.length <= 1} on:click={() => removeSequencePose(index)}>删除</button>
                      </div>
                    </div>
                  {/each}
                </div>
                {#if missingActionPoseIds.length > 0}
                  <div class="rig-warning-line">序列引用了不存在的姿势，播放时会跳过这些项。</div>
                {/if}
              </div>
              <label class="rig-field">
                标签
                <input value={selectedRigAction.tags.join(", ")} on:input={(event) => updateRigAction({ tags: commaList((event.currentTarget as HTMLInputElement).value) })} />
              </label>
              <div class="rig-action-row compact-two">
                <button type="button" class="rig-secondary" on:click={createRigAction}>新建动作+姿势</button>
                <button type="button" class="rig-secondary danger" on:click={deleteRigAction}>删除动作</button>
              </div>
            </div>
          </details>
        {/if}
      </section>

      {:else if activeInspectorTab === "feedback"}
      <section>
        <div class="rig-section-head">
          <h2>目标反应</h2>
          <span>{selectedRigAction?.id || "-"}</span>
        </div>

        {#if selectedRigAction}
          <div class="rig-feedback-grid">
            {#each reactionResults as result}
              <div class="rig-feedback-card">
                <span>{combatResultLabels[result]}</span>
                <strong>{targetReactionLabels[targetReactionFor(result)]}</strong>
                <code>{result} -> {targetReactionFor(result)}</code>
              </div>
            {/each}
          </div>

          <details class="rig-advanced-editor">
            <summary>编辑目标反应</summary>
            <div class="rig-advanced-editor-body">
              <div class="rig-reaction-grid">
                {#each reactionResults as result}
                  <label>
                    {combatResultLabels[result]}
                    <select value={targetReactionFor(result)} on:change={(event) => updateTargetReaction(result, selectInput(event) as TargetReaction)}>
                      <option value="none">无目标反应</option>
                      <option value="hit">受击</option>
                      <option value="dodge">闪避</option>
                      <option value="parry">招架</option>
                      <option value="effect">效果反应</option>
                    </select>
                  </label>
                {/each}
              </div>
            </div>
          </details>

          <div class="rig-section-head compact">
            <h2>特效</h2>
            <button type="button" class="rig-secondary" on:click={addVfx}>添加特效</button>
          </div>
          {#if selectedRigAction.vfx.length > 0}
            <div class="rig-vfx-summary-list">
              {#each selectedRigAction.vfx as vfx}
                <div class="rig-vfx-summary-card">
                  <strong>{vfxKindLabels[vfx.kind]}</strong>
                  <span>{vfx.variant}</span>
                  <code>{vfxSummary(vfx)}</code>
                </div>
              {/each}
            </div>
          {:else}
            <p class="rig-empty-note">无特效</p>
          {/if}

          <details class="rig-advanced-editor">
            <summary>编辑特效</summary>
            <div class="rig-advanced-editor-body">
              <div class="rig-vfx-list">
                {#each selectedRigAction.vfx as vfx, index}
                  <div class="rig-vfx-row">
                    <select value={vfx.kind} on:change={(event) => updateVfx(index, { kind: selectInput(event) as BattleActionDefinition["vfx"][number]["kind"] })}>
                      <option value="trail">轨迹</option>
                      <option value="impact">命中特效</option>
                      <option value="parry">招架特效</option>
                      <option value="aura">气场</option>
                      <option value="heal">治疗</option>
                    </select>
                    <input value={vfx.variant} on:input={(event) => updateVfx(index, { variant: (event.currentTarget as HTMLInputElement).value })} />
                    <select value={vfx.anchor} on:change={(event) => updateVfx(index, { anchor: selectInput(event) as BattleActionDefinition["vfx"][number]["anchor"] })}>
                      <option value="actor">出招者</option>
                      <option value="target">目标</option>
                      <option value="center">场中央</option>
                    </select>
                    <button type="button" class="rig-secondary" on:click={() => removeVfx(index)}>删除</button>
                  </div>
                {/each}
              </div>
            </div>
          </details>
        {/if}
      </section>

      {:else if activeInspectorTab === "pose"}
      <section>
        <div class="rig-section-head">
          <h2>姿势资源</h2>
          <span>{saveStatus}</span>
        </div>

        <div class="rig-pose-identity">
          <div class="rig-field">
            <span>姿势 ID</span>
            <form class="rig-pose-id-row" on:submit={submitSelectedPoseId}>
              <input name="poseId" bind:value={poseIdDraft} />
              <button type="submit" class="rig-secondary">重命名</button>
            </form>
          </div>
          <div class="rig-field">
            <span>名称</span>
            <form class="rig-pose-id-row" on:submit={submitSelectedPoseName}>
              <input name="poseName" bind:value={poseNameDraft} />
              <button type="submit" class="rig-secondary">更新</button>
            </form>
          </div>
          <div class="rig-field">
            <span>单段时长</span>
            <form class="rig-pose-id-row" on:submit={submitSelectedPoseDuration}>
              <input name="poseDuration" type="number" min="40" step="20" bind:value={poseDurationDraft} />
              <button type="submit" class="rig-secondary">更新</button>
            </form>
          </div>
        </div>

        <div class="rig-pose-relation">
          <div>
            <span>姿势名称</span>
            <strong>{poseLabelById(selectedPoseId, rig)}</strong>
          </div>
          <div>
            <span>引用动作</span>
            <strong>{poseUsageCount(selectedPoseId)} 个</strong>
          </div>
        </div>

        {#if selectedRigAction}
          <div class="rig-pose-relation">
            <div>
              <span>当前动作</span>
              <strong>{selectedRigAction.id}</strong>
            </div>
            <div>
              <span>主姿势</span>
              <strong>{selectedRigAction.poseId}</strong>
            </div>
          </div>

          <div class="rig-chip-block">
            <span>动作使用姿势</span>
            <div class="rig-pose-button-list">
              {#each selectedActionPoseIds as poseId}
                <button type="button" class:active={poseId === selectedPoseId} disabled={!rig?.poses[poseId]} on:click={() => selectPose(poseId)}>
                  <strong>{poseLabelById(poseId, rig)}</strong>
                  <code>{poseId}</code>
                </button>
              {/each}
            </div>
          </div>

          <div class="rig-pose-workflow">
            <div>
              <span>当前姿势</span>
              <strong>{poseLabelById(selectedPoseId, rig)}</strong>
              <code>{selectedPoseId}</code>
            </div>
            <button type="button" class="rig-secondary" on:click={copySelectedPoseToCurrentAction}>复制当前姿势给动作</button>
            <button type="button" class="rig-secondary" on:click={createBlankPoseForCurrentAction}>新建空白姿势给动作</button>
            <button type="button" class="rig-primary" disabled={!canSaveProjectPoses} on:click={saveProjectPoses}>保存姿势库</button>
            <button type="button" class="rig-primary" on:click={saveRigActions}>保存动作引用</button>
          </div>
        {/if}

        <label class="rig-field">
          姿势骨骼
          <select bind:value={selectedBoneId}>
            {#each resolvedRig?.bones || [] as bone (bone.id)}
              <option value={bone.id}>{bone.id}</option>
            {/each}
          </select>
        </label>

        <div class="rig-slider-field">
          <span>旋转</span>
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
          关键点
          <select bind:value={selectedAnchorId}>
            {#each poseAnchorOptions(resolvedRig) as anchor (anchor.id)}
              <option value={anchor.id}>{anchor.id}</option>
            {/each}
          </select>
        </label>

        <div class="rig-pair-fields">
          <label>
            关键点 X
            <input
              type="number"
              step="0.5"
              value={round(selectedAnchor?.definition.x ?? 0)}
              on:input={(event) => updateSelectedPoseAnchor({ x: numericInput(event) })}
            />
          </label>
          <label>
            关键点 Y
            <input
              type="number"
              step="0.5"
              value={round(selectedAnchor?.definition.y ?? 0)}
              on:input={(event) => updateSelectedPoseAnchor({ y: numericInput(event) })}
            />
          </label>
        </div>

        <button type="button" class="rig-secondary" on:click={resetSelectedPoseAnchor}>重置关键点</button>
        <button type="button" class="rig-secondary" on:click={resetSelectedBone}>重置骨骼</button>
      </section>

      {:else if activeInspectorTab === "anchor"}
      <section>
        <div class="rig-section-head">
          <h2>锚点</h2>
          <button type="button" class="rig-secondary" on:click={resetSelectedAnchor}>重置</button>
        </div>

        <label class="rig-field">
          锚点
          <select bind:value={selectedAnchorId}>
            {#each resolvedRig?.anchors || [] as anchor (anchor.id)}
              <option value={anchor.id}>{anchor.id}</option>
            {/each}
          </select>
        </label>

        <label class="rig-field">
          所属骨骼
          <select value={selectedAnchor?.definition.boneId || ""} on:change={(event) => updateSelectedAnchor({ boneId: selectInput(event) })}>
            {#each resolvedRig?.bones || [] as bone (bone.id)}
              <option value={bone.id}>{bone.id}</option>
            {/each}
          </select>
        </label>

        <label class="rig-field">
          拖拽控制骨骼
          <select
            value={selectedAnchor?.definition.handleBoneId || ""}
            on:change={(event) => updateSelectedAnchor({ handleBoneId: optionalSelectInput(event) })}
          >
            <option value="">无</option>
            {#each resolvedRig?.bones || [] as bone (bone.id)}
              <option value={bone.id}>{bone.id}</option>
            {/each}
          </select>
        </label>

        <div class="rig-pair-fields">
          <label>
            锚点 X
            <input
              type="number"
              step="0.5"
              value={round(selectedAnchor?.definition.x ?? 0)}
              on:input={(event) => updateSelectedAnchor({ x: numericInput(event) })}
            />
          </label>
          <label>
            锚点 Y
            <input
              type="number"
              step="0.5"
              value={round(selectedAnchor?.definition.y ?? 0)}
              on:input={(event) => updateSelectedAnchor({ y: numericInput(event) })}
            />
          </label>
        </div>
      </section>

      {:else if activeInspectorTab === "binding"}
      <section>
        <div class="rig-section-head">
          <h2>贴图绑定</h2>
          <span>{saveStatus}</span>
        </div>

        <div class="rig-binding-card">
          <span>当前部件</span>
          <strong>{selectedBinding?.name || "-"}</strong>
          <code>{selectedBinding?.id || "-"}</code>
          <small>
            {bindingKindLabels[selectedBinding?.definition.kind || ""] || selectedBinding?.definition.kind || "-"}
            · {round(selectedBinding?.definition.width ?? 0)} x {round(selectedBinding?.definition.height ?? 0)}
            · opacity {round(selectedBinding?.definition.opacity ?? 0)}
          </small>
        </div>

        <div class="rig-binding-actions">
          <button type="button" class="rig-secondary" disabled={!resolvedRig?.bindingsById["prop.sword"]} on:click={selectWeaponBinding}>选中武器</button>
          <button type="button" class="rig-secondary danger" disabled={!selectedBindingId} on:click={hideSelectedBindingForCurrentPose}>隐藏当前姿势</button>
          <button type="button" class="rig-secondary danger" disabled={!selectedBindingId} on:click={hideSelectedBindingForAllPoses}>隐藏所有姿势</button>
          <button type="button" class="rig-secondary" disabled={!selectedBindingId} on:click={applySwordBindingTemplate}>套用细剑模板</button>
        </div>

        <button type="button" class="rig-primary" disabled={!canSaveProjectPoses} on:click={saveProjectPoses}>保存姿势库</button>
        <button type="button" class="rig-secondary" on:click={resetSelectedBinding}>重置当前姿势绑定</button>

        <label class="rig-field">
          绑定
          <select bind:value={selectedBindingId}>
            {#each resolvedRig?.bindings || [] as binding (binding.id)}
              <option value={binding.id}>{binding.id}</option>
            {/each}
          </select>
        </label>

        <div class="rig-pair-fields">
          <label>
            类型
            <select value={selectedBinding?.definition.kind || ""} on:change={(event) => updateSelectedBinding({ kind: selectInput(event) as BindingPose["kind"] })}>
              {#if selectedBinding?.definition.kind === "mesh"}
                <option value="mesh">蒙皮图片</option>
              {/if}
              {#each editableBindingKinds as kind}
                <option value={kind}>{bindingKindLabels[kind]}</option>
              {/each}
            </select>
          </label>
          <label>
            绘制层级
            <input
              type="number"
              step="1"
              value={round(selectedBinding?.definition.drawOrder ?? 0)}
              on:input={(event) => updateSelectedBinding({ drawOrder: numericInput(event) })}
            />
          </label>
        </div>

        <label class="rig-field">
          挂接锚点
          <select value={selectedBinding?.definition.anchorId || ""} on:change={(event) => updateSelectedBinding({ anchorId: selectInput(event) })}>
            {#each bindingAnchorOptions(rig) as anchor (anchor.id)}
              <option value={anchor.id}>{anchor.id}</option>
            {/each}
          </select>
        </label>

        <div class="rig-pair-fields">
          <label>
            偏移 X
            <input
              type="number"
              step="0.5"
              value={round(selectedBinding?.definition.offsetX ?? 0)}
              on:input={(event) => updateSelectedBinding({ offsetX: numericInput(event) })}
            />
          </label>
          <label>
            偏移 Y
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
            旋转
            <input
              type="number"
              step="1"
              value={round(selectedBinding?.definition.rotation ?? 0)}
              on:input={(event) => updateSelectedBinding({ rotation: numericInput(event) })}
            />
          </label>
          <label>
            透明度
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
            宽度
            <input
              type="number"
              step="0.5"
              value={round(selectedBinding?.definition.width ?? 0)}
              on:input={(event) => updateSelectedBinding({ width: numericInput(event) })}
            />
          </label>
          <label>
            高度
            <input
              type="number"
              step="0.1"
              value={round(selectedBinding?.definition.height ?? 0)}
              on:input={(event) => updateSelectedBinding({ height: numericInput(event) })}
            />
          </label>
        </div>

        <div class="rig-pair-fields">
          <label>
            颜色
            <input
              value={selectedBinding?.definition.color || ""}
              placeholder="rgba(...) / #rrggbb"
              on:input={(event) => updateSelectedBinding({ color: (event.currentTarget as HTMLInputElement).value })}
            />
          </label>
          <label>
            描边
            <input
              value={selectedBinding?.definition.strokeColor || ""}
              placeholder="可选"
              on:input={(event) => updateSelectedBinding({ strokeColor: (event.currentTarget as HTMLInputElement).value })}
            />
          </label>
        </div>

        <div class="rig-pair-fields">
          <label>
            Pivot X
            <input
              type="number"
              min="0"
              max="1"
              step="0.05"
              value={round(selectedBinding?.definition.pivotX ?? 0.5)}
              on:input={(event) => updateSelectedBinding({ pivotX: numericInput(event) })}
            />
          </label>
          <label>
            Pivot Y
            <input
              type="number"
              min="0"
              max="1"
              step="0.05"
              value={round(selectedBinding?.definition.pivotY ?? 0.5)}
              on:input={(event) => updateSelectedBinding({ pivotY: numericInput(event) })}
            />
          </label>
        </div>

        <label class="rig-field">
          图片路径
          <input
            value={selectedBinding?.definition.image || ""}
            placeholder="可访问的 PNG/WebP 路径；类型设为图片后使用"
            on:change={(event) => updateSelectedBinding({ image: (event.currentTarget as HTMLInputElement).value })}
          />
        </label>

        <div class="rig-pair-fields">
          <label>
            缩放 X
            <input
              type="number"
              step="0.05"
              value={round(selectedBinding?.definition.scaleX ?? 1)}
              on:input={(event) => updateSelectedBinding({ scaleX: numericInput(event) })}
            />
          </label>
          <label>
            缩放 Y
            <input
              type="number"
              step="0.05"
              value={round(selectedBinding?.definition.scaleY ?? 1)}
              on:input={(event) => updateSelectedBinding({ scaleY: numericInput(event) })}
            />
          </label>
        </div>
      </section>

      {:else if activeInspectorTab === "map"}
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

      {:else}
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
      {/if}
    </aside>
  </main>
  {:else}
  <main class="rig-mapping-layout">
    <section class="rig-mapping-panel">
      <div class="rig-section-head">
        <h2>武功文件</h2>
        <span>{martialStatus}</span>
      </div>
      <div class="rig-filter-grid">
        <select value={selectedMartialFile} aria-label="Martial art file" on:change={(event) => selectMartialFile(selectInput(event))}>
          {#each martialFiles as file (file.file)}
            <option value={file.file}>{file.file}</option>
          {/each}
        </select>
        <select value={currentMartialArt?.key || ""} aria-label="Martial art" on:change={(event) => selectMartialArt(selectInput(event))}>
          {#each martialParse.arts as art (art.key)}
            <option value={art.key}>{art.name}</option>
          {/each}
        </select>
      </div>

      {#if martialParse.error}
        <p class="rig-error-text">{martialParse.error}</p>
      {/if}

      {#if currentMartialArt}
        <div class="rig-mapping-summary">
          <strong>{currentMartialArt.id}</strong>
          <span>{currentMartialArt.data.type || "-"}</span>
          <span>{poolIds(currentMartialArt.data).length} pools</span>
          <span>{mappingTargets.length} moves</span>
        </div>
      {/if}

      <div class="rig-entry-list mapping-targets">
        {#each mappingTargets as target (target.key)}
          <button type="button" class:active={target.key === currentMappingTarget?.key} on:click={() => (selectedMappingTargetKey = target.key)}>
            <strong>{target.name}</strong>
            <span>{target.kind === "attack_moves" ? "普攻" : "主动技能"} / {target.id}</span>
            <small>
              {target.data.animation?.action ? `action: ${target.data.animation.action}` : `pool: ${target.data.animation?.pool || "-"}`}
            </small>
          </button>
        {/each}
      </div>
    </section>

    <section class="rig-mapping-panel">
      <div class="rig-section-head">
        <h2>招式映射</h2>
        <span>{currentMappingTarget?.id || "-"}</span>
      </div>
      {#if currentMappingTarget && currentMartialArt}
        <div class="rig-segmented wide">
          <button type="button" class:active={!!selectedTargetAnimation().pool} on:click={() => setTargetAnimationMode("pool")}>Pool</button>
          <button type="button" class:active={!!selectedTargetAnimation().action} on:click={() => setTargetAnimationMode("action")}>Action</button>
        </div>

        {#if selectedTargetAnimation().action}
          <label class="rig-field">
            固定动作
            <select value={selectedTargetAnimation().action} on:change={(event) => updateTargetAnimation({ action: selectInput(event) })}>
              {#each actionIdsForStyle(styleFilterForArt(currentMartialArt.data.type)) as actionId}
                <option value={actionId}>{actionId}</option>
              {/each}
            </select>
          </label>
        {:else}
          <label class="rig-field">
            动作池
            <select value={selectedTargetAnimation().pool || poolIds(currentMartialArt.data)[0] || ""} on:change={(event) => updateTargetAnimation({ pool: selectInput(event) })}>
              {#each poolIds(currentMartialArt.data) as poolId}
                <option value={poolId}>{poolId}</option>
              {/each}
            </select>
          </label>
        {/if}

        <label class="rig-field">
          匹配标签
          <input value={(selectedTargetAnimation().tags || []).join(", ")} on:input={(event) => updateTargetTags((event.currentTarget as HTMLInputElement).value)} />
        </label>

        <button type="button" class="rig-primary" on:click={saveSelectedMartialArt}>Save Martial YAML</button>
      {/if}
    </section>

    <section class="rig-mapping-panel">
      <div class="rig-section-head">
        <h2>动作池</h2>
        <button type="button" class="rig-secondary" on:click={addAnimationPool}>New Pool</button>
      </div>
      {#if currentMartialArt}
        <div class="rig-pair-fields">
          <label>
            Pool
            <select value={currentPoolId} on:change={(event) => (selectedMappingPoolId = selectInput(event))}>
              {#each poolIds(currentMartialArt.data) as poolId}
                <option value={poolId}>{poolId}</option>
              {/each}
            </select>
          </label>
          <label>
            Pool ID
            <input value={currentPoolId} on:change={(event) => renameAnimationPool((event.currentTarget as HTMLInputElement).value)} />
          </label>
        </div>

        <div class="rig-pool-entry-list">
          {#each currentPoolEntries as entry, index}
            <div class="rig-pool-entry-row">
              <select value={entry.action || ""} on:change={(event) => updatePoolEntry(index, { action: selectInput(event) })}>
                {#each actionIdsForStyle(styleFilterForArt(currentMartialArt.data.type)) as actionId}
                  <option value={actionId}>{actionId}</option>
                {/each}
              </select>
              <input type="number" min="1" step="1" value={entry.weight || 1} on:input={(event) => updatePoolEntry(index, { weight: numericInput(event) })} />
              <input value={(entry.tags || []).join(", ")} on:input={(event) => updatePoolEntry(index, { tags: commaList((event.currentTarget as HTMLInputElement).value) })} />
              <button type="button" class="rig-secondary" on:click={() => removePoolEntry(index)}>Del</button>
            </div>
          {/each}
        </div>
        <div class="rig-action-row compact-two">
          <button type="button" class="rig-secondary" on:click={addPoolEntry}>Add Candidate</button>
          <button type="button" class="rig-secondary" on:click={deleteAnimationPool}>Delete Pool</button>
        </div>
      {/if}
    </section>

    <section class="rig-mapping-panel yaml">
      <div class="rig-section-head">
        <h2>YAML</h2>
        <span>{selectedMartial?.file || "-"}</span>
      </div>
      {#if selectedMartial}
        <textarea
          class="rig-yaml-editor"
          spellcheck="false"
          value={selectedMartial.text}
          on:input={(event) => updateSelectedMartialText((event.currentTarget as HTMLTextAreaElement).value)}
        ></textarea>
      {/if}
    </section>
  </main>
  {/if}
</div>
