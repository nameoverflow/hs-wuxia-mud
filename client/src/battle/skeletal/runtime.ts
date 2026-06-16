import { applyToPoint, composeTransform, interpolateAngle, lerp, matrixRotationDegrees, multiply, translation } from "./math";
import type {
  AnchorDefinition,
  AnchorPose,
  BindingDefinition,
  BindingPose,
  BoneDefinition,
  BonePose,
  PoseOverrides,
  ResolvedAnchor,
  ResolvedBinding,
  ResolvedBone,
  ResolvedRig,
  SkeletalPoseDefinition,
  SkeletonRigDefinition
} from "./types";

const emptyPose: SkeletalPoseDefinition = { id: "empty", name: "Empty", bones: {} };

export function resolveRig(
  definition: SkeletonRigDefinition,
  pose: SkeletalPoseDefinition | null | undefined = emptyPose,
  overrides: PoseOverrides = { bones: {}, anchors: {}, bindings: {} }
): ResolvedRig {
  const activePose = pose || emptyPose;
  const bonesById: Record<string, ResolvedBone> = {};
  const bones: ResolvedBone[] = [];

  for (const bone of definition.bones) {
    const local = resolveBonePose(bone, activePose.bones[bone.id], overrides.bones[bone.id]);
    const localMatrix = composeTransform(local.x, local.y, local.rotation, local.scaleX, local.scaleY);
    const parent = bone.parentId ? bonesById[bone.parentId] : null;
    const matrix = parent ? multiply(parent.matrix, localMatrix) : localMatrix;
    const start = applyToPoint(matrix, { x: 0, y: 0 });
    const end = applyToPoint(matrix, { x: bone.length, y: 0 });
    const resolved: ResolvedBone = {
      id: bone.id,
      name: bone.name,
      definition: bone,
      parentId: bone.parentId,
      local,
      matrix,
      worldRotation: matrixRotationDegrees(matrix),
      start,
      end
    };
    bonesById[bone.id] = resolved;
    bones.push(resolved);
  }

  const bindingDefs = definition.bindings.map((binding) =>
    resolveBindingDefinition(binding, activePose.bindings?.[binding.id], overrides.bindings[binding.id])
  );
  const bindingIdsByAnchor = bindingDefs.reduce<Record<string, string[]>>((acc, binding) => {
    acc[binding.anchorId] = [...(acc[binding.anchorId] || []), binding.id];
    return acc;
  }, {});

  const anchorsById: Record<string, ResolvedAnchor> = {};
  const anchors = definition.anchors.map((anchorDefinition) => {
    const anchor = resolveAnchorDefinition(anchorDefinition, activePose.anchors?.[anchorDefinition.id], overrides.anchors?.[anchorDefinition.id]);
    const bone = bonesById[anchor.boneId];
    const matrix = bone ? multiply(bone.matrix, translation(anchor.x, anchor.y)) : translation(anchor.x, anchor.y);
    const resolved: ResolvedAnchor = {
      id: anchor.id,
      name: anchor.name,
      definition: anchor,
      matrix,
      position: applyToPoint(matrix, { x: 0, y: 0 }),
      bindingIds: bindingIdsByAnchor[anchor.id] || []
    };
    anchorsById[anchor.id] = resolved;
    return resolved;
  });

  const bindingsById: Record<string, ResolvedBinding> = {};
  const bindings = bindingDefs
    .map((binding) => {
      const anchor = anchorsById[binding.anchorId] || anchors[0];
      const matrix = anchor
        ? multiply(anchor.matrix, composeTransform(binding.offsetX, binding.offsetY, binding.rotation, binding.scaleX, binding.scaleY))
        : composeTransform(binding.offsetX, binding.offsetY, binding.rotation, binding.scaleX, binding.scaleY);
      const resolved: ResolvedBinding = {
        id: binding.id,
        name: binding.name,
        definition: binding,
        anchor,
        matrix,
        position: applyToPoint(matrix, { x: 0, y: 0 })
      };
      bindingsById[binding.id] = resolved;
      return resolved;
    })
    .sort((a, b) => a.definition.drawOrder - b.definition.drawOrder);

  return {
    definition,
    pose: activePose,
    bones,
    anchors,
    bindings,
    bonesById,
    anchorsById,
    bindingsById
  };
}

export function interpolatePose(
  from: SkeletalPoseDefinition,
  to: SkeletalPoseDefinition,
  t: number,
  id = `${from.id}-to-${to.id}`
): SkeletalPoseDefinition {
  const boneIds = new Set([...Object.keys(from.bones), ...Object.keys(to.bones)]);
  const anchorIds = new Set([...Object.keys(from.anchors || {}), ...Object.keys(to.anchors || {})]);
  const bindingIds = new Set([...Object.keys(from.bindings || {}), ...Object.keys(to.bindings || {})]);
  const bones: Record<string, BonePose> = {};
  const anchors: Record<string, AnchorPose> = {};
  const bindings: Record<string, BindingPose> = {};

  for (const boneId of boneIds) {
    const a = from.bones[boneId] || {};
    const b = to.bones[boneId] || {};
    bones[boneId] = {
      x: interpolateOptional(a.x, b.x, t),
      y: interpolateOptional(a.y, b.y, t),
      rotation: interpolateOptionalAngle(a.rotation, b.rotation, t),
      scaleX: interpolateOptional(a.scaleX, b.scaleX, t),
      scaleY: interpolateOptional(a.scaleY, b.scaleY, t)
    };
  }

  for (const anchorId of anchorIds) {
    const a = from.anchors?.[anchorId] || {};
    const b = to.anchors?.[anchorId] || {};
    anchors[anchorId] = {
      boneId: b.boneId || a.boneId,
      x: interpolateOptional(a.x, b.x, t),
      y: interpolateOptional(a.y, b.y, t),
      handleBoneId: b.handleBoneId === undefined ? a.handleBoneId : b.handleBoneId
    };
  }

  for (const bindingId of bindingIds) {
    const a = from.bindings?.[bindingId] || {};
    const b = to.bindings?.[bindingId] || {};
    bindings[bindingId] = {
      anchorId: b.anchorId || a.anchorId,
      offsetX: interpolateOptional(a.offsetX, b.offsetX, t),
      offsetY: interpolateOptional(a.offsetY, b.offsetY, t),
      rotation: interpolateOptionalAngle(a.rotation, b.rotation, t),
      scaleX: interpolateOptional(a.scaleX, b.scaleX, t),
      scaleY: interpolateOptional(a.scaleY, b.scaleY, t),
      opacity: interpolateOptional(a.opacity, b.opacity, t)
    };
  }

  return {
    id,
    name: `${from.name} -> ${to.name}`,
    durationMs: to.durationMs || from.durationMs,
    bones,
    anchors,
    bindings
  };
}

function resolveAnchorDefinition(definition: AnchorDefinition, pose: AnchorPose | undefined, override: AnchorPose | undefined): AnchorDefinition {
  const handleBoneId =
    override?.handleBoneId === undefined
      ? pose?.handleBoneId === undefined
        ? definition.handleBoneId
        : pose.handleBoneId || undefined
      : override.handleBoneId || undefined;
  return {
    ...definition,
    boneId: override?.boneId ?? pose?.boneId ?? definition.boneId,
    x: overrideValue(definition.x, pose?.x, override?.x),
    y: overrideValue(definition.y, pose?.y, override?.y),
    handleBoneId
  };
}

function resolveBonePose(definition: BoneDefinition, pose: BonePose | undefined, override: BonePose | undefined): Required<BonePose> {
  return {
    x: overrideValue(definition.x, pose?.x, override?.x),
    y: overrideValue(definition.y, pose?.y, override?.y),
    rotation: overrideValue(definition.rotation, pose?.rotation, override?.rotation),
    scaleX: overrideValue(definition.scaleX ?? 1, pose?.scaleX, override?.scaleX),
    scaleY: overrideValue(definition.scaleY ?? 1, pose?.scaleY, override?.scaleY)
  };
}

function resolveBindingDefinition(
  definition: BindingDefinition,
  pose: BindingPose | undefined,
  override: BindingPose | undefined
): BindingDefinition {
  return {
    ...definition,
    anchorId: override?.anchorId ?? pose?.anchorId ?? definition.anchorId,
    offsetX: overrideValue(definition.offsetX, pose?.offsetX, override?.offsetX),
    offsetY: overrideValue(definition.offsetY, pose?.offsetY, override?.offsetY),
    rotation: overrideValue(definition.rotation, pose?.rotation, override?.rotation),
    scaleX: overrideValue(definition.scaleX, pose?.scaleX, override?.scaleX),
    scaleY: overrideValue(definition.scaleY, pose?.scaleY, override?.scaleY),
    opacity: overrideValue(definition.opacity, pose?.opacity, override?.opacity)
  };
}

function overrideValue(base: number, poseValue: number | undefined, override: number | undefined) {
  return override ?? poseValue ?? base;
}

function interpolateOptional(a: number | undefined, b: number | undefined, t: number) {
  if (a === undefined && b === undefined) return undefined;
  return lerp(a ?? b ?? 0, b ?? a ?? 0, t);
}

function interpolateOptionalAngle(a: number | undefined, b: number | undefined, t: number) {
  if (a === undefined && b === undefined) return undefined;
  return interpolateAngle(a ?? b ?? 0, b ?? a ?? 0, t);
}
