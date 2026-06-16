export interface Vec2 {
  x: number;
  y: number;
}

export interface Mat2D {
  a: number;
  b: number;
  c: number;
  d: number;
  e: number;
  f: number;
}

export interface RigCanvasDefinition {
  width: number;
  height: number;
  baseline: number;
}

export interface BoneDefinition {
  id: string;
  name: string;
  parentId: string | null;
  x: number;
  y: number;
  length: number;
  rotation: number;
  scaleX?: number;
  scaleY?: number;
  color?: string;
}

export interface AnchorDefinition {
  id: string;
  name: string;
  boneId: string;
  x: number;
  y: number;
  kind?: "joint" | "socket" | "target" | "root";
  color?: string;
  handleBoneId?: string;
}

export type BindingKind = "image" | "capsule" | "circle" | "line" | "target";

export interface MeshDeformKeypoint {
  anchorId: string;
  sourceX: number;
  sourceY: number;
  radius: number;
  sourceRadius?: number;
}

export interface MeshDeformDefinition {
  keypoints: [MeshDeformKeypoint, MeshDeformKeypoint, MeshDeformKeypoint];
  segments?: number;
}

export interface BindingDefinition {
  id: string;
  name: string;
  kind: BindingKind | "mesh";
  anchorId: string;
  drawOrder: number;
  offsetX: number;
  offsetY: number;
  rotation: number;
  scaleX: number;
  scaleY: number;
  opacity: number;
  width: number;
  height: number;
  image?: string;
  pivotX?: number;
  pivotY?: number;
  deform?: MeshDeformDefinition;
  color?: string;
  strokeColor?: string;
  tags?: string[];
}

export interface BonePose {
  x?: number;
  y?: number;
  rotation?: number;
  scaleX?: number;
  scaleY?: number;
}

export interface BindingPose {
  anchorId?: string;
  offsetX?: number;
  offsetY?: number;
  rotation?: number;
  scaleX?: number;
  scaleY?: number;
  opacity?: number;
}

export interface AnchorPose {
  boneId?: string;
  x?: number;
  y?: number;
  handleBoneId?: string | null;
}

export interface SkeletalPoseDefinition {
  id: string;
  name: string;
  durationMs?: number;
  bones: Record<string, BonePose>;
  anchors?: Record<string, AnchorPose>;
  bindings?: Record<string, BindingPose>;
}

export interface PoseOverrides {
  bones: Record<string, BonePose>;
  anchors?: Record<string, AnchorPose>;
  bindings: Record<string, BindingPose>;
}

export interface SkeletonRigDefinition {
  id: string;
  name: string;
  canvas: RigCanvasDefinition;
  bones: BoneDefinition[];
  anchors: AnchorDefinition[];
  bindings: BindingDefinition[];
  poses: Record<string, SkeletalPoseDefinition>;
}

export interface ResolvedBone {
  id: string;
  name: string;
  definition: BoneDefinition;
  parentId: string | null;
  local: Required<BonePose>;
  matrix: Mat2D;
  worldRotation: number;
  start: Vec2;
  end: Vec2;
}

export interface ResolvedAnchor {
  id: string;
  name: string;
  definition: AnchorDefinition;
  matrix: Mat2D;
  position: Vec2;
  bindingIds: string[];
}

export interface ResolvedBinding {
  id: string;
  name: string;
  definition: BindingDefinition;
  anchor: ResolvedAnchor;
  matrix: Mat2D;
  position: Vec2;
}

export interface ResolvedRig {
  definition: SkeletonRigDefinition;
  pose: SkeletalPoseDefinition;
  bones: ResolvedBone[];
  anchors: ResolvedAnchor[];
  bindings: ResolvedBinding[];
  bonesById: Record<string, ResolvedBone>;
  anchorsById: Record<string, ResolvedAnchor>;
  bindingsById: Record<string, ResolvedBinding>;
}

export interface SkeletalRenderOptions {
  showBones: boolean;
  showAnchors: boolean;
  showBindings: boolean;
  showImages: boolean;
  showSkin: boolean;
  showLabels: boolean;
  selectedBoneId?: string | null;
  selectedAnchorId?: string | null;
  selectedBindingId?: string | null;
  zoom?: number;
  panX?: number;
  panY?: number;
  background?: string;
}

export interface RigViewport {
  scale: number;
  offsetX: number;
  offsetY: number;
}

export interface AnimationRigEntry {
  id: string;
  actionId: string;
  clipId: string;
  label: string;
  profile: "male" | "female";
  style: "sword" | "fist";
  poseId: string;
  sprite: string | null;
  tags: string[];
  durationMs: number;
}
