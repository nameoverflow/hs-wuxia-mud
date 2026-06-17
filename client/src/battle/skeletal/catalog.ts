import { battleActions, spriteForClip } from "../animationCatalog";
import type { BattleActionDefinition, CombatStyle, VisualProfile } from "../animationTypes";
import type {
  AnimationRigEntry,
  BindingDefinition,
  BoneDefinition,
  MeshDeformDefinition,
  SkeletalPoseDefinition,
  SkeletonRigDefinition
} from "./types";

import segmentedPoseData from "./data/segmented-v12-poses.json";
import segmentedArmBack from "../../assets/battle/actors/segmented/v12/arm_back.png";
import segmentedArmFront from "../../assets/battle/actors/segmented/v12/arm_front.png";
import segmentedHead from "../../assets/battle/actors/segmented/v12/head.png";
import segmentedLegBack from "../../assets/battle/actors/segmented/v12/leg_back.png";
import segmentedLegFront from "../../assets/battle/actors/segmented/v12/leg_front.png";
import segmentedPonytail from "../../assets/battle/actors/segmented/v12/ponytail.png";
import segmentedTorso from "../../assets/battle/actors/segmented/v12/torso.png";

const canvas = { width: 256, height: 192, baseline: 176 };
const spritePivotY = canvas.baseline / canvas.height;
const profiles: VisualProfile[] = ["male", "female"];
const segmentedSourceScale = 0.2;
const segmentedTorsoScale = segmentedSourceScale;
const segmentedHeadScale = segmentedSourceScale;
const segmentedArmScale = segmentedSourceScale;
const segmentedBackArmScale = segmentedSourceScale;
const segmentedFrontLegScale = segmentedSourceScale;
const segmentedBackLegScale = segmentedSourceScale;
const segmentedPonytailScale = segmentedSourceScale;
const segmentedHipsX = 4.02;
const segmentedHipsY = -37.325;
const segmentedSpineLength = 50.271;
const segmentedSpineRotation = -90.755;
const segmentedHeadRotation = 90.755;
const segmentedTorsoBindingRotation = 90.755;
const segmentedFrontShoulderLength = 9.897;
const segmentedRearShoulderLength = 7.286;
const segmentedFrontHipLength = 9.839;
const segmentedRearHipLength = 7.144;
const segmentedFrontArmLength = 27.02;
const segmentedRearArmLength = 30.84;
const segmentedFrontHandLength = 10.413;
const segmentedRearHandLength = 7.837;
const segmentedFrontLegLength = 48.915;
const segmentedRearLegLength = 49.789;
const segmentedFrontFootLength = 10.276;
const segmentedRearFootLength = 9.359;

const bones: BoneDefinition[] = [
  { id: "root", name: "root", parentId: null, x: 128, y: 176, length: 0, rotation: 0, color: "#c8a75a" },
  { id: "hips", name: "hips", parentId: "root", x: 0, y: -48, length: 24, rotation: 0, color: "#c8a75a" },
  { id: "spine", name: "spine", parentId: "hips", x: 0, y: 0, length: 54, rotation: -90, color: "#54c6b1" },
  { id: "neck", name: "neck", parentId: "spine", x: 54, y: 0, length: 7, rotation: 0, color: "#54c6b1" },
  { id: "head", name: "head", parentId: "neck", x: 7, y: 0, length: 0, rotation: 0, color: "#54c6b1" },
  { id: "frontUpperArm", name: "front upper arm", parentId: "spine", x: 36, y: -3, length: 35, rotation: 112, color: "#6b96d8" },
  { id: "frontForearm", name: "front forearm", parentId: "frontUpperArm", x: 35, y: 0, length: 34, rotation: 8, color: "#6b96d8" },
  { id: "rearUpperArm", name: "rear upper arm", parentId: "spine", x: 31, y: 5, length: 31, rotation: 76, color: "#6b96d8" },
  { id: "rearForearm", name: "rear forearm", parentId: "rearUpperArm", x: 31, y: 0, length: 28, rotation: 64, color: "#6b96d8" },
  { id: "frontThigh", name: "front thigh", parentId: "hips", x: 4, y: 2, length: 41, rotation: 63, color: "#78b56b" },
  { id: "frontShin", name: "front shin", parentId: "frontThigh", x: 41, y: 0, length: 37, rotation: -31, color: "#78b56b" },
  { id: "rearThigh", name: "rear thigh", parentId: "hips", x: -4, y: 2, length: 39, rotation: 116, color: "#78b56b" },
  { id: "rearShin", name: "rear shin", parentId: "rearThigh", x: 39, y: 0, length: 35, rotation: 32, color: "#78b56b" },
  { id: "sword", name: "sword", parentId: "frontForearm", x: 34, y: 0, length: 68, rotation: -4, color: "#d35d56" }
];

const anchors = [
  { id: "root", name: "Root", boneId: "root", x: 0, y: 0, kind: "root" as const, color: "#c8a75a" },
  { id: "hips", name: "Hips", boneId: "hips", x: 0, y: 0, kind: "joint" as const, color: "#c8a75a", handleBoneId: "hips" },
  { id: "spineBase", name: "Spine base", boneId: "spine", x: 0, y: 0, kind: "joint" as const, color: "#54c6b1" },
  { id: "chest", name: "Chest", boneId: "spine", x: 54, y: 0, kind: "joint" as const, color: "#54c6b1", handleBoneId: "spine" },
  { id: "head", name: "Head", boneId: "head", x: 0, y: 0, kind: "joint" as const, color: "#54c6b1" },
  { id: "frontShoulder", name: "Front shoulder", boneId: "frontUpperArm", x: 0, y: 0, kind: "joint" as const, color: "#6b96d8" },
  { id: "frontElbow", name: "Front elbow", boneId: "frontUpperArm", x: 35, y: 0, kind: "joint" as const, color: "#6b96d8", handleBoneId: "frontUpperArm" },
  { id: "frontHand", name: "Front hand", boneId: "frontForearm", x: 34, y: 0, kind: "socket" as const, color: "#d35d56", handleBoneId: "frontForearm" },
  { id: "rearShoulder", name: "Rear shoulder", boneId: "rearUpperArm", x: 0, y: 0, kind: "joint" as const, color: "#6b96d8" },
  { id: "rearElbow", name: "Rear elbow", boneId: "rearUpperArm", x: 31, y: 0, kind: "joint" as const, color: "#6b96d8", handleBoneId: "rearUpperArm" },
  { id: "rearHand", name: "Rear hand", boneId: "rearForearm", x: 28, y: 0, kind: "socket" as const, color: "#6b96d8", handleBoneId: "rearForearm" },
  { id: "frontHip", name: "Front hip", boneId: "frontThigh", x: 0, y: 0, kind: "joint" as const, color: "#78b56b" },
  { id: "frontKnee", name: "Front knee", boneId: "frontThigh", x: 41, y: 0, kind: "joint" as const, color: "#78b56b", handleBoneId: "frontThigh" },
  { id: "frontFoot", name: "Front foot", boneId: "frontShin", x: 37, y: 0, kind: "socket" as const, color: "#78b56b", handleBoneId: "frontShin" },
  { id: "rearHip", name: "Rear hip", boneId: "rearThigh", x: 0, y: 0, kind: "joint" as const, color: "#78b56b" },
  { id: "rearKnee", name: "Rear knee", boneId: "rearThigh", x: 39, y: 0, kind: "joint" as const, color: "#78b56b", handleBoneId: "rearThigh" },
  { id: "rearFoot", name: "Rear foot", boneId: "rearShin", x: 35, y: 0, kind: "socket" as const, color: "#78b56b", handleBoneId: "rearShin" },
  { id: "weaponGrip", name: "Weapon grip", boneId: "sword", x: 0, y: 0, kind: "socket" as const, color: "#d35d56" },
  { id: "weaponTip", name: "Weapon tip", boneId: "sword", x: 68, y: 0, kind: "target" as const, color: "#d35d56", handleBoneId: "sword" },
  { id: "impact", name: "Impact", boneId: "root", x: 78, y: -86, kind: "target" as const, color: "#d35d56" }
];

const segmentedBones: BoneDefinition[] = [
  { id: "root", name: "root", parentId: null, x: 128, y: 176, length: 0, rotation: 0, color: "#c8a75a" },
  { id: "hips", name: "hips", parentId: "root", x: segmentedHipsX, y: segmentedHipsY, length: 0, rotation: 0, color: "#c8a75a" },
  { id: "spine", name: "spine", parentId: "hips", x: 0, y: 0, length: segmentedSpineLength, rotation: segmentedSpineRotation, color: "#54c6b1" },
  { id: "head", name: "head", parentId: "spine", x: segmentedSpineLength + 3.1, y: -2.6, length: 0, rotation: segmentedHeadRotation, color: "#54c6b1" },
  { id: "ponytailBase", name: "ponytail base", parentId: "head", x: -4.283, y: -32.373, length: 3.554, rotation: -131.016, color: "#c8a75a" },
  { id: "ponytailMid", name: "ponytail mid", parentId: "ponytailBase", x: 3.554, y: 0, length: 13.056, rotation: -40.987, color: "#c8a75a" },
  { id: "ponytailLower", name: "ponytail lower", parentId: "ponytailMid", x: 13.056, y: 0, length: 38.251, rotation: -84.092, color: "#c8a75a" },
  { id: "ponytailTail", name: "ponytail tail", parentId: "ponytailLower", x: 38.251, y: 0, length: 27.699, rotation: 12.133, color: "#c8a75a" },
  { id: "frontArmRoot", name: "front arm root", parentId: "spine", x: 49, y: -2.66, length: 8.7, rotation: 127.5, color: "#6b96d8" },
  { id: "frontArm", name: "front arm", parentId: "frontArmRoot", x: 8.7, y: 0, length: segmentedFrontArmLength, rotation: -18.353, color: "#6b96d8" },
  { id: "rearArmRoot", name: "rear arm root", parentId: "spine", x: 50.65, y: -1.35, length: 6.9, rotation: 240.5, color: "#6b96d8" },
  { id: "rearArm", name: "rear arm", parentId: "rearArmRoot", x: 6.9, y: 0, length: segmentedRearArmLength, rotation: -35.941, color: "#6b96d8" },
  { id: "frontLegRoot", name: "front leg root", parentId: "hips", x: -5.8, y: -21, length: 14.5, rotation: 86, color: "#78b56b" },
  { id: "frontLeg", name: "front leg", parentId: "frontLegRoot", x: 14.5, y: 0, length: segmentedFrontLegLength, rotation: -27.2, color: "#78b56b" },
  { id: "rearLegRoot", name: "rear leg root", parentId: "hips", x: -6.2, y: -22, length: 13, rotation: 92, color: "#78b56b" },
  { id: "rearLeg", name: "rear leg", parentId: "rearLegRoot", x: 13, y: 0, length: segmentedRearLegLength, rotation: 24.2, color: "#78b56b" },
  { id: "sword", name: "straight sword", parentId: "frontArm", x: 30.078, y: -9.953, length: 58, rotation: 0, color: "#d35d56" },
  { id: "impact", name: "impact", parentId: "root", x: 76, y: -86, length: 0, rotation: 0, color: "#d35d56" }
];

const segmentedAnchors = [
  { id: "root", name: "Root", boneId: "root", x: 0, y: 0, kind: "root" as const, color: "#c8a75a" },
  { id: "hips", name: "Hips", boneId: "hips", x: 0, y: 0, kind: "joint" as const, color: "#c8a75a", handleBoneId: "hips" },
  { id: "spineBase", name: "Spine base", boneId: "spine", x: 0, y: 0, kind: "joint" as const, color: "#54c6b1" },
  { id: "chest", name: "Chest", boneId: "spine", x: 28, y: 0, kind: "joint" as const, color: "#54c6b1" },
  { id: "neck", name: "Neck", boneId: "spine", x: segmentedSpineLength, y: 0, kind: "joint" as const, color: "#54c6b1", handleBoneId: "spine" },
  { id: "head", name: "Head", boneId: "head", x: 0, y: 0, kind: "joint" as const, color: "#54c6b1" },
  { id: "ponytailRoot", name: "Ponytail root", boneId: "head", x: -4.283, y: -32.373, kind: "joint" as const, color: "#c8a75a" },
  { id: "ponytailBase", name: "Ponytail fixed base", boneId: "ponytailBase", x: 3.554, y: 0, kind: "joint" as const, color: "#c8a75a", handleBoneId: "ponytailBase" },
  { id: "ponytailMid", name: "Ponytail mid", boneId: "ponytailMid", x: 13.056, y: 0, kind: "joint" as const, color: "#c8a75a", handleBoneId: "ponytailMid" },
  { id: "ponytailLower", name: "Ponytail lower", boneId: "ponytailLower", x: 38.251, y: 0, kind: "joint" as const, color: "#c8a75a", handleBoneId: "ponytailLower" },
  { id: "ponytailTip", name: "Ponytail tip", boneId: "ponytailTail", x: 27.699, y: 0, kind: "socket" as const, color: "#c8a75a", handleBoneId: "ponytailTail" },
  { id: "frontArmRoot", name: "Front arm hidden root", boneId: "frontArmRoot", x: 0, y: 0, kind: "joint" as const, color: "#6b96d8", handleBoneId: "frontArmRoot" },
  { id: "rearArmRoot", name: "Rear arm hidden root", boneId: "rearArmRoot", x: 0, y: 0, kind: "joint" as const, color: "#6b96d8", handleBoneId: "rearArmRoot" },
  { id: "frontShoulder", name: "Front shoulder", boneId: "frontArm", x: 0, y: 0, kind: "joint" as const, color: "#6b96d8", handleBoneId: "frontArm" },
  { id: "frontElbow", name: "Front elbow", boneId: "frontArm", x: 16.402, y: 9.891, kind: "joint" as const, color: "#6b96d8" },
  { id: "frontWrist", name: "Front wrist", boneId: "frontArm", x: segmentedFrontArmLength, y: 0, kind: "socket" as const, color: "#6b96d8", handleBoneId: "frontArm" },
  { id: "frontHand", name: "Front hand", boneId: "frontArm", x: 30.078, y: -9.953, kind: "socket" as const, color: "#6b96d8", handleBoneId: "frontArm" },
  { id: "rearShoulder", name: "Rear shoulder", boneId: "rearArm", x: 0, y: 0, kind: "joint" as const, color: "#6b96d8", handleBoneId: "rearArm" },
  { id: "rearElbow", name: "Rear elbow", boneId: "rearArm", x: 19.527, y: 4.843, kind: "joint" as const, color: "#6b96d8" },
  { id: "rearWrist", name: "Rear wrist", boneId: "rearArm", x: segmentedRearArmLength, y: 0, kind: "socket" as const, color: "#6b96d8", handleBoneId: "rearArm" },
  { id: "rearHand", name: "Rear hand", boneId: "rearArm", x: 35.732, y: -6.122, kind: "socket" as const, color: "#6b96d8", handleBoneId: "rearArm" },
  { id: "frontLegRoot", name: "Front leg hidden root", boneId: "frontLegRoot", x: 0, y: 0, kind: "joint" as const, color: "#78b56b", handleBoneId: "frontLegRoot" },
  { id: "rearLegRoot", name: "Rear leg hidden root", boneId: "rearLegRoot", x: 0, y: 0, kind: "joint" as const, color: "#78b56b", handleBoneId: "rearLegRoot" },
  { id: "frontHip", name: "Front hip", boneId: "frontLeg", x: 0, y: 0, kind: "joint" as const, color: "#78b56b", handleBoneId: "frontLeg" },
  { id: "frontKnee", name: "Front knee", boneId: "frontLeg", x: 24.078, y: -5.988, kind: "joint" as const, color: "#78b56b" },
  { id: "frontAnkle", name: "Front ankle", boneId: "frontLeg", x: segmentedFrontLegLength, y: 0, kind: "socket" as const, color: "#78b56b", handleBoneId: "frontLeg" },
  { id: "frontFoot", name: "Front foot", boneId: "frontLeg", x: 55.801, y: -7.627, kind: "socket" as const, color: "#78b56b", handleBoneId: "frontLeg" },
  { id: "rearHip", name: "Rear hip", boneId: "rearLeg", x: 0, y: 0, kind: "joint" as const, color: "#78b56b", handleBoneId: "rearLeg" },
  { id: "rearKnee", name: "Rear knee", boneId: "rearLeg", x: 25.417, y: 0.281, kind: "joint" as const, color: "#78b56b" },
  { id: "rearAnkle", name: "Rear ankle", boneId: "rearLeg", x: segmentedRearLegLength, y: 0, kind: "socket" as const, color: "#78b56b", handleBoneId: "rearLeg" },
  { id: "rearFoot", name: "Rear foot", boneId: "rearLeg", x: 47.358, y: -9.038, kind: "socket" as const, color: "#78b56b", handleBoneId: "rearLeg" },
  { id: "weaponGrip", name: "Weapon grip", boneId: "sword", x: 0, y: 0, kind: "socket" as const, color: "#d35d56" },
  { id: "weaponTip", name: "Weapon tip", boneId: "sword", x: 58, y: 0, kind: "target" as const, color: "#d35d56", handleBoneId: "sword" },
  { id: "impact", name: "Impact", boneId: "impact", x: 0, y: 0, kind: "target" as const, color: "#d35d56", handleBoneId: "impact" }
];

export const skeletalPoseLibrary: Record<string, SkeletalPoseDefinition> = {
  idle: { id: "idle", name: "Idle", bones: {} },
  stab: {
    id: "stab",
    name: "Stab",
    durationMs: 760,
    bones: {
      root: { x: 132 },
      spine: { rotation: -96 },
      frontUpperArm: { rotation: 92 },
      frontForearm: { rotation: -8 },
      rearUpperArm: { rotation: 138 },
      rearForearm: { rotation: -26 },
      frontThigh: { rotation: 54 },
      frontShin: { rotation: -22 },
      rearThigh: { rotation: 132 },
      rearShin: { rotation: 18 },
      sword: { rotation: -3 }
    }
  },
  slash: {
    id: "slash",
    name: "Slash",
    durationMs: 840,
    bones: {
      root: { x: 130, y: 175 },
      spine: { rotation: -104 },
      frontUpperArm: { rotation: 154 },
      frontForearm: { rotation: -44 },
      rearUpperArm: { rotation: 66 },
      rearForearm: { rotation: 72 },
      frontThigh: { rotation: 57 },
      frontShin: { rotation: -26 },
      rearThigh: { rotation: 122 },
      rearShin: { rotation: 26 },
      sword: { rotation: 22 }
    }
  },
  uppercut: {
    id: "uppercut",
    name: "Uppercut",
    durationMs: 900,
    bones: {
      root: { x: 130, y: 174 },
      spine: { rotation: -86 },
      frontUpperArm: { rotation: 190 },
      frontForearm: { rotation: -36 },
      rearUpperArm: { rotation: 95 },
      rearForearm: { rotation: 48 },
      frontThigh: { rotation: 69 },
      frontShin: { rotation: -38 },
      rearThigh: { rotation: 118 },
      rearShin: { rotation: 30 },
      sword: { rotation: 24 }
    }
  },
  guard: {
    id: "guard",
    name: "Guard",
    durationMs: 720,
    bones: {
      root: { x: 126 },
      spine: { rotation: -92 },
      frontUpperArm: { rotation: 148 },
      frontForearm: { rotation: -86 },
      rearUpperArm: { rotation: 42 },
      rearForearm: { rotation: 96 },
      frontThigh: { rotation: 70 },
      frontShin: { rotation: -38 },
      rearThigh: { rotation: 104 },
      rearShin: { rotation: 44 },
      sword: { rotation: 42 }
    }
  },
  punch: {
    id: "punch",
    name: "Punch",
    durationMs: 720,
    bones: {
      root: { x: 132 },
      spine: { rotation: -99 },
      frontUpperArm: { rotation: 95 },
      frontForearm: { rotation: -3 },
      rearUpperArm: { rotation: 132 },
      rearForearm: { rotation: -28 },
      frontThigh: { rotation: 57 },
      frontShin: { rotation: -28 },
      rearThigh: { rotation: 130 },
      rearShin: { rotation: 16 }
    }
  },
  heavy: {
    id: "heavy",
    name: "Heavy",
    durationMs: 820,
    bones: {
      root: { x: 131, y: 176 },
      spine: { rotation: -110 },
      frontUpperArm: { rotation: 122 },
      frontForearm: { rotation: -14 },
      rearUpperArm: { rotation: 166 },
      rearForearm: { rotation: -34 },
      frontThigh: { rotation: 48 },
      frontShin: { rotation: -14 },
      rearThigh: { rotation: 136 },
      rearShin: { rotation: 12 }
    }
  },
  kick: {
    id: "kick",
    name: "Kick",
    durationMs: 860,
    bones: {
      root: { x: 130, y: 169 },
      spine: { rotation: -104 },
      frontUpperArm: { rotation: 135 },
      frontForearm: { rotation: -62 },
      rearUpperArm: { rotation: 74 },
      rearForearm: { rotation: 52 },
      frontThigh: { rotation: -10 },
      frontShin: { rotation: 16 },
      rearThigh: { rotation: 122 },
      rearShin: { rotation: 26 }
    }
  },
  healing_palm: {
    id: "healing_palm",
    name: "Healing palm",
    durationMs: 720,
    bones: {
      root: { x: 126, y: 174 },
      spine: { rotation: -88 },
      frontUpperArm: { rotation: 118 },
      frontForearm: { rotation: -36 },
      rearUpperArm: { rotation: 48 },
      rearForearm: { rotation: 104 },
      frontThigh: { rotation: 70 },
      frontShin: { rotation: -38 },
      rearThigh: { rotation: 110 },
      rearShin: { rotation: 38 }
    }
  },
  hurt: {
    id: "hurt",
    name: "Hurt",
    durationMs: 640,
    bones: {
      root: { x: 120, y: 176 },
      spine: { rotation: -116 },
      frontUpperArm: { rotation: 144 },
      frontForearm: { rotation: 24 },
      rearUpperArm: { rotation: 36 },
      rearForearm: { rotation: 52 },
      frontThigh: { rotation: 76 },
      frontShin: { rotation: -22 },
      rearThigh: { rotation: 104 },
      rearShin: { rotation: 48 }
    }
  },
  dodge: {
    id: "dodge",
    name: "Dodge",
    durationMs: 700,
    bones: {
      root: { x: 106, y: 178 },
      spine: { rotation: -76 },
      frontUpperArm: { rotation: 152 },
      frontForearm: { rotation: -24 },
      rearUpperArm: { rotation: 58 },
      rearForearm: { rotation: 44 },
      frontThigh: { rotation: 86 },
      frontShin: { rotation: -50 },
      rearThigh: { rotation: 134 },
      rearShin: { rotation: 8 }
    }
  },
  parry: {
    id: "parry",
    name: "Parry",
    durationMs: 720,
    bones: {
      root: { x: 124 },
      spine: { rotation: -88 },
      frontUpperArm: { rotation: 174 },
      frontForearm: { rotation: -82 },
      rearUpperArm: { rotation: 76 },
      rearForearm: { rotation: 72 },
      frontThigh: { rotation: 68 },
      frontShin: { rotation: -34 },
      rearThigh: { rotation: 112 },
      rearShin: { rotation: 36 },
      sword: { rotation: 76 }
    }
  },
  effect: {
    id: "effect",
    name: "Effect",
    durationMs: 560,
    bones: {
      root: { y: 172 },
      spine: { rotation: -90 },
      frontUpperArm: { rotation: 132 },
      frontForearm: { rotation: -48 },
      rearUpperArm: { rotation: 48 },
      rearForearm: { rotation: 96 }
    }
  }
};

const segmentedPoseLibrary = segmentedPoseData as Record<string, SkeletalPoseDefinition>;

const segmentedBaseEntries: AnimationRigEntry[] = profiles.map((profile) => ({
  id: `segmented.v12.${profile}`,
  actionId: "segmented.v12",
  clipId: "segmented-rig-v12",
  label: `segmented rig v12 / ${profile}`,
  profile,
  style: "fist",
  poseId: "idle",
  sprite: null,
  tags: ["segmented", "part-rig", "v12", profile],
  durationMs: 1080
}));

const segmentedSwordActions = [
  { id: "thrust", poseId: "swordThrust", label: "jian thrust", durationMs: 760, tags: ["attack", "jian", "thrust"] },
  { id: "chop", poseId: "swordChop", label: "jian chop", durationMs: 780, tags: ["attack", "jian", "chop"] },
  { id: "rising_cut", poseId: "swordLiao", label: "jian rising cut", durationMs: 760, tags: ["attack", "jian", "rising-cut"] },
  { id: "hurt", poseId: "swordHurt", label: "jian hurt", durationMs: 520, tags: ["reaction", "hurt", "jian"] },
  { id: "dodge", poseId: "swordDodge", label: "jian dodge", durationMs: 560, tags: ["reaction", "dodge", "jian"] },
  { id: "parry", poseId: "swordParry", label: "jian parry", durationMs: 560, tags: ["reaction", "parry", "guard", "jian"] }
] as const;

const segmentedSwordEntries: AnimationRigEntry[] = segmentedSwordActions.flatMap((action) =>
  profiles.map((profile) => ({
    id: `segmented.v12.sword.${action.id}.${profile}`,
    actionId: `segmented.v12.sword.${action.id}`,
    clipId: "segmented-rig-v12-sword",
    label: `${action.label} / ${profile}`,
    profile,
    style: "sword" as const,
    poseId: action.poseId,
    sprite: null,
    tags: ["segmented", "part-rig", "v12", profile, "sword", ...action.tags],
    durationMs: action.durationMs
  }))
);

const segmentedRigEntries: AnimationRigEntry[] = [...segmentedBaseEntries, ...segmentedSwordEntries];

export const skeletalAnimationEntries: AnimationRigEntry[] = [
  ...segmentedRigEntries,
  ...Object.values(battleActions).flatMap((action) => {
  const style = styleForAction(action);
  const poseId = poseForAction(action);
  return profiles.map((profile) => ({
    id: `${action.id}.${profile}`,
    actionId: action.id,
    clipId: action.clipId,
    label: `${action.id} / ${profile}`,
    profile,
    style,
    poseId,
    sprite: action.clipId ? spriteForClip(action.clipId, profile, style) : null,
    tags: action.tags,
    durationMs: action.durationMs
  }));
  })
];

export function createBattleActorRig(entry: AnimationRigEntry): SkeletonRigDefinition {
  if (entry.tags.includes("part-rig")) return createSegmentedActorRig(entry);
  return {
    id: `rig.${entry.id}`,
    name: entry.label,
    canvas,
    bones,
    anchors,
    bindings: bindingsForEntry(entry),
    poses: skeletalPoseLibrary
  };
}

function createSegmentedActorRig(entry: AnimationRigEntry): SkeletonRigDefinition {
  return {
    id: `rig.${entry.id}`,
    name: entry.label,
    canvas,
    bones: segmentedBones,
    anchors: segmentedAnchors,
    bindings: segmentedBindingsForEntry(entry),
    poses: segmentedPoseLibrary
  };
}

function segmentedBindingsForEntry(entry: AnimationRigEntry): BindingDefinition[] {
  const ponytailOpacity = entry.profile === "female" ? 1 : 0;
  const swordOpacity = entry.style === "sword" ? 0.95 : 0;
  return [
    partBinding(
      "part.leg_back",
      "rear leg",
      "rearLegRoot",
      -32,
      segmentedLegBack,
      221,
      314,
      segmentedBackLegScale,
      0.86264,
      0.07438,
      1,
      -94,
      limbMesh(
        221 * segmentedBackLegScale,
        314 * segmentedBackLegScale,
        [
          { anchorId: "rearLegRoot", x: 0.86264, y: 0.07438, radius: 5.4, sourceRadius: 5.4 },
          { anchorId: "rearHip", x: 0.74831, y: 0.1548, radius: 4.8, sourceRadius: 4.8 },
          { anchorId: "rearKnee", x: 0.48826, y: 0.5158, radius: 4.2, sourceRadius: 4.2 },
          { anchorId: "rearAnkle", x: 0.25008, y: 0.86585, radius: 4.8, sourceRadius: 4.8 },
          { anchorId: "rearFoot", x: 0.4578, y: 0.89478, radius: 5.2, sourceRadius: 5.2 }
        ],
        24
      )
    ),
    partBinding(
      "part.arm_back",
      "rear arm",
      "rearArmRoot",
      -30,
      segmentedArmBack,
      164,
      238,
      segmentedBackArmScale,
      0.8539,
      0.10044,
      1,
      -14,
      limbMesh(
        164 * segmentedBackArmScale,
        238 * segmentedBackArmScale,
        [
          { anchorId: "rearArmRoot", x: 0.8539, y: 0.10044, radius: 4.8, sourceRadius: 4.8 },
          { anchorId: "rearShoulder", x: 0.65662, y: 0.1708, radius: 4.4, sourceRadius: 4.4 },
          { anchorId: "rearElbow", x: 0.25726, y: 0.4916, radius: 3.8, sourceRadius: 3.8 },
          { anchorId: "rearWrist", x: 0.23424, y: 0.74964, radius: 4.3, sourceRadius: 4.3 },
          { anchorId: "rearHand", x: 0.334, y: 0.89925, radius: 5.1, sourceRadius: 5.1 }
        ],
        24
      )
    ),
    partBinding(
      "part.torso",
      "torso",
      "spineBase",
      -34,
      segmentedTorso,
      150,
      299,
      segmentedTorsoScale,
      0.57844,
      0.99,
      1,
      segmentedTorsoBindingRotation
    ),
    partBinding(
      "part.ponytail",
      "ponytail",
      "ponytailRoot",
      -24,
      segmentedPonytail,
      278,
      475,
      segmentedPonytailScale,
      0.89223,
      0.17931,
      ponytailOpacity,
      0,
      limbMesh(
        278 * segmentedPonytailScale,
        475 * segmentedPonytailScale,
        [
          { anchorId: "ponytailRoot", x: 0.89223, y: 0.17931, radius: 3.4, sourceRadius: 3.4 },
          { anchorId: "ponytailBase", x: 0.85028, y: 0.15109, radius: 3.2, sourceRadius: 3.2 },
          { anchorId: "ponytailMid", x: 0.61775, y: 0.13197, radius: 4.7, sourceRadius: 4.7 },
          { anchorId: "ponytailLower", x: 0.45242, y: 0.52281, radius: 5.2, sourceRadius: 5.2 },
          { anchorId: "ponytailTip", x: 0.23373, y: 0.78479, radius: 3.2, sourceRadius: 3.2 }
        ],
        30,
        1.65,
        3.2
      )
    ),
    partBinding("part.head", "head", "head", -2, segmentedHead, 184, 217, segmentedHeadScale, 0.45755, 0.845, 1),
    partBinding(
      "part.leg_front",
      "front leg",
      "frontLegRoot",
      -8,
      segmentedLegFront,
      269,
      310,
      segmentedFrontLegScale,
      0.08697,
      0.12136,
      1,
      -88,
      limbMesh(
        269 * segmentedFrontLegScale,
        310 * segmentedFrontLegScale,
        [
          { anchorId: "frontLegRoot", x: 0.08697, y: 0.12136, radius: 5.4, sourceRadius: 5.4 },
          { anchorId: "frontHip", x: 0.25054, y: 0.19233, radius: 4.8, sourceRadius: 4.8 },
          { anchorId: "frontKnee", x: 0.57791, y: 0.47419, radius: 4.2, sourceRadius: 4.2 },
          { anchorId: "frontAnkle", x: 0.72233, y: 0.86675, radius: 4.8, sourceRadius: 4.8 },
          { anchorId: "frontFoot", x: 0.90994, y: 0.89786, radius: 5.2, sourceRadius: 5.2 }
        ],
        24
      )
    ),
    partBinding(
      "part.arm_front",
      "front arm",
      "frontArmRoot",
      -6,
      segmentedArmFront,
      249,
      181,
      segmentedArmScale,
      0.09199,
      0.16001,
      1,
      -8,
      limbMesh(
        249 * segmentedArmScale,
        181 * segmentedArmScale,
        [
          { anchorId: "frontArmRoot", x: 0.09199, y: 0.16001, radius: 4.8, sourceRadius: 4.8 },
          { anchorId: "frontShoulder", x: 0.25285, y: 0.32054, radius: 4.4, sourceRadius: 4.4 },
          { anchorId: "frontElbow", x: 0.50672, y: 0.71801, radius: 3.8, sourceRadius: 3.8 },
          { anchorId: "frontWrist", x: 0.77001, y: 0.54628, radius: 4.3, sourceRadius: 4.3 },
          { anchorId: "frontHand", x: 0.88899, y: 0.30976, radius: 5.1, sourceRadius: 5.1 }
        ],
        24
      )
    ),
    debugBinding("prop.sword", "straight sword", "line", "weaponGrip", 18, 0, 0, 0, swordOpacity, 42, 2.8, "rgba(232, 38, 30, 0.98)"),
    debugBinding("target.impact", "impact target", "target", "impact", 40, 0, 0, 0, 0.78, 18, 18, "rgba(211, 93, 86, 0.88)")
  ];
}

function partBinding(
  id: string,
  name: string,
  anchorId: string,
  drawOrder: number,
  image: string,
  sourceWidth: number,
  sourceHeight: number,
  scale: number,
  pivotX: number,
  pivotY: number,
  opacity: number,
  rotation = 0,
  deform?: MeshDeformDefinition
): BindingDefinition {
  return {
    id,
    name,
    kind: deform ? "mesh" : "image",
    anchorId,
    drawOrder,
    offsetX: 0,
    offsetY: 0,
    rotation,
    scaleX: 1,
    scaleY: 1,
    opacity,
    width: sourceWidth * scale,
    height: sourceHeight * scale,
    image,
    pivotX,
    pivotY,
    deform,
    color: "#f8d42d",
    tags: ["skin", "segmented"]
  };
}

function limbMesh(
  width: number,
  height: number,
  keypoints: { anchorId: string; x: number; y: number; radius: number; sourceRadius?: number }[],
  segments?: number,
  gridSize = 2,
  influence = 2.6
): MeshDeformDefinition {
  const deform: MeshDeformDefinition = {
    algorithm: "skinned",
    gridSize,
    influence,
    keypoints: keypoints.map((keypoint) => ({
      anchorId: keypoint.anchorId,
      sourceX: keypoint.x * width,
      sourceY: keypoint.y * height,
      radius: keypoint.radius,
      ...(keypoint.sourceRadius === undefined ? {} : { sourceRadius: keypoint.sourceRadius })
    }))
  };
  if (segments !== undefined) deform.segments = segments;
  return deform;
}

function bindingsForEntry(entry: AnimationRigEntry): BindingDefinition[] {
  const sourceOpacity = entry.sprite ? 0.34 : 0;
  const swordOpacity = entry.style === "sword" ? 0.72 : 0;
  return [
    {
      id: "source.frame",
      name: "source frame",
      kind: "image",
      anchorId: "root",
      drawOrder: -40,
      offsetX: 0,
      offsetY: 0,
      rotation: 0,
      scaleX: 1,
      scaleY: 1,
      opacity: sourceOpacity,
      width: canvas.width,
      height: canvas.height,
      pivotX: 0.5,
      pivotY: spritePivotY,
      image: entry.sprite || undefined,
      tags: ["source"]
    },
    debugBinding("shadow", "ground shadow", "circle", "root", -30, 0, 2, 0, 0.34, 78, 12, "#000000"),
    debugBinding("skin.torso", "torso skin", "capsule", "spineBase", -10, 0, 0, 0, 0.58, 58, 22, "rgba(232, 225, 207, 0.52)"),
    debugBinding("skin.head", "head skin", "circle", "head", -9, 0, 0, 0, 0.64, 27, 27, "rgba(232, 225, 207, 0.56)"),
    debugBinding("skin.frontUpperArm", "front upper arm skin", "capsule", "frontShoulder", 0, 0, 0, 0, 0.48, 35, 9, "rgba(107, 150, 216, 0.56)"),
    debugBinding("skin.frontForearm", "front forearm skin", "capsule", "frontElbow", 0, 0, 0, 0, 0.5, 34, 8, "rgba(107, 150, 216, 0.6)"),
    debugBinding("skin.rearUpperArm", "rear upper arm skin", "capsule", "rearShoulder", 0, 0, 0, 0, 0.36, 31, 8, "rgba(107, 150, 216, 0.42)"),
    debugBinding("skin.rearForearm", "rear forearm skin", "capsule", "rearElbow", 0, 0, 0, 0, 0.38, 28, 7, "rgba(107, 150, 216, 0.46)"),
    debugBinding("skin.frontThigh", "front thigh skin", "capsule", "frontHip", 0, 0, 0, 0, 0.44, 41, 11, "rgba(120, 181, 107, 0.5)"),
    debugBinding("skin.frontShin", "front shin skin", "capsule", "frontKnee", 0, 0, 0, 0, 0.48, 37, 9, "rgba(120, 181, 107, 0.55)"),
    debugBinding("skin.rearThigh", "rear thigh skin", "capsule", "rearHip", 0, 0, 0, 0, 0.34, 39, 10, "rgba(120, 181, 107, 0.36)"),
    debugBinding("skin.rearShin", "rear shin skin", "capsule", "rearKnee", 0, 0, 0, 0, 0.38, 35, 8, "rgba(120, 181, 107, 0.42)"),
    debugBinding("prop.sword", "sword binding", "line", "weaponGrip", 0, 0, 0, 0, swordOpacity, 68, 2.8, "rgba(211, 93, 86, 0.88)"),
    debugBinding("target.impact", "impact target", "target", "impact", 0, 0, 0, 0, 0.78, 18, 18, "rgba(211, 93, 86, 0.88)")
  ];
}

function debugBinding(
  id: string,
  name: string,
  kind: BindingDefinition["kind"],
  anchorId: string,
  drawOrder: number,
  offsetX: number,
  offsetY: number,
  rotation: number,
  opacity: number,
  width: number,
  height: number,
  color: string
): BindingDefinition {
  return {
    id,
    name,
    kind,
    anchorId,
    drawOrder,
    offsetX,
    offsetY,
    rotation,
    scaleX: 1,
    scaleY: 1,
    opacity,
    width,
    height,
    color,
    strokeColor: "rgba(0, 0, 0, 0.32)",
    tags: ["skin"]
  };
}

function styleForAction(action: BattleActionDefinition): CombatStyle {
  const search = `${action.id} ${action.clipId} ${action.tags.join(" ")}`;
  return search.includes("sword") ? "sword" : "fist";
}

function poseForAction(action: BattleActionDefinition) {
  const search = `${action.id} ${action.clipId} ${action.tags.join(" ")}`;
  if (search.includes("stab")) return "stab";
  if (search.includes("slash")) return "slash";
  if (search.includes("uppercut")) return "uppercut";
  if (search.includes("guard") || search.includes("stance")) return "guard";
  if (search.includes("punch")) return "punch";
  if (search.includes("heavy")) return "heavy";
  if (search.includes("kick")) return "kick";
  if (search.includes("healing_palm") || search.includes("heal")) return "healing_palm";
  if (search.includes("hurt")) return "hurt";
  if (search.includes("dodge")) return "dodge";
  if (search.includes("parry")) return "parry";
  if (search.includes("effect") || search.includes("dot") || search.includes("hot")) return "effect";
  return "idle";
}
