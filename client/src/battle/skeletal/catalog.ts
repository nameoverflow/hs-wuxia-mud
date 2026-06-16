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

import segmentedArmBack from "../../assets/battle/actors/segmented/v4/arm_back.png";
import segmentedArmFront from "../../assets/battle/actors/segmented/v4/arm_front.png";
import segmentedHead from "../../assets/battle/actors/segmented/v4/head.png";
import segmentedLegFront from "../../assets/battle/actors/segmented/v4/leg_front.png";
import segmentedPonytail from "../../assets/battle/actors/segmented/v4/ponytail.png";
import segmentedTorso from "../../assets/battle/actors/segmented/v4/torso.png";

const canvas = { width: 256, height: 192, baseline: 176 };
const spritePivotY = canvas.baseline / canvas.height;
const profiles: VisualProfile[] = ["male", "female"];
const segmentedTorsoScale = 0.14;
const segmentedHeadScale = 0.09;
const segmentedArmScale = 0.14;
const segmentedBackArmScale = 0.14;
const segmentedLegScale = 0.13;
const segmentedPonytailScale = 0.095;

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
  { id: "hips", name: "hips", parentId: "root", x: -2, y: -58, length: 0, rotation: 0, color: "#c8a75a" },
  { id: "spine", name: "spine", parentId: "hips", x: 0, y: 0, length: 49, rotation: -90, color: "#54c6b1" },
  { id: "head", name: "head", parentId: "spine", x: 49, y: 4, length: 0, rotation: 90, color: "#54c6b1" },
  { id: "ponytailBase", name: "ponytail base", parentId: "head", x: -8, y: -8, length: 14, rotation: 178, color: "#c8a75a" },
  { id: "ponytailTail", name: "ponytail tail", parentId: "ponytailBase", x: 14, y: 0, length: 32, rotation: 0, color: "#c8a75a" },
  { id: "frontArm", name: "front arm", parentId: "spine", x: 34, y: 14, length: 45, rotation: 178, color: "#6b96d8" },
  { id: "rearArm", name: "rear arm", parentId: "spine", x: 32, y: -9, length: 43, rotation: 184, color: "#6b96d8" },
  { id: "frontLeg", name: "front leg", parentId: "hips", x: 7, y: 1, length: 58, rotation: 88, color: "#78b56b" },
  { id: "rearLeg", name: "rear leg", parentId: "hips", x: -7, y: 1, length: 57, rotation: 94, color: "#78b56b" },
  { id: "sword", name: "straight sword", parentId: "frontArm", x: 45, y: 0, length: 58, rotation: 0, color: "#d35d56" },
  { id: "impact", name: "impact", parentId: "root", x: 76, y: -86, length: 0, rotation: 0, color: "#d35d56" }
];

const segmentedAnchors = [
  { id: "root", name: "Root", boneId: "root", x: 0, y: 0, kind: "root" as const, color: "#c8a75a" },
  { id: "hips", name: "Hips", boneId: "hips", x: 0, y: 0, kind: "joint" as const, color: "#c8a75a", handleBoneId: "hips" },
  { id: "spineBase", name: "Spine base", boneId: "spine", x: 0, y: 0, kind: "joint" as const, color: "#54c6b1" },
  { id: "chest", name: "Chest", boneId: "spine", x: 34, y: 2, kind: "joint" as const, color: "#54c6b1" },
  { id: "neck", name: "Neck", boneId: "spine", x: 49, y: 4, kind: "joint" as const, color: "#54c6b1", handleBoneId: "spine" },
  { id: "head", name: "Head", boneId: "head", x: 0, y: 0, kind: "joint" as const, color: "#54c6b1" },
  { id: "ponytailRoot", name: "Ponytail root", boneId: "head", x: -8, y: -8, kind: "joint" as const, color: "#c8a75a" },
  { id: "ponytailMid", name: "Ponytail fixed base", boneId: "ponytailBase", x: 14, y: 0, kind: "joint" as const, color: "#c8a75a" },
  { id: "ponytailTip", name: "Ponytail tip", boneId: "ponytailTail", x: 32, y: 0, kind: "socket" as const, color: "#c8a75a", handleBoneId: "ponytailTail" },
  { id: "frontShoulder", name: "Front shoulder", boneId: "frontArm", x: 0, y: 0, kind: "joint" as const, color: "#6b96d8", handleBoneId: "frontArm" },
  { id: "frontElbow", name: "Front elbow", boneId: "frontArm", x: 23, y: 0, kind: "joint" as const, color: "#6b96d8" },
  { id: "frontWrist", name: "Front wrist", boneId: "frontArm", x: 45, y: 0, kind: "socket" as const, color: "#6b96d8", handleBoneId: "frontArm" },
  { id: "rearShoulder", name: "Rear shoulder", boneId: "rearArm", x: 0, y: 0, kind: "joint" as const, color: "#6b96d8", handleBoneId: "rearArm" },
  { id: "rearElbow", name: "Rear elbow", boneId: "rearArm", x: 22, y: 0, kind: "joint" as const, color: "#6b96d8" },
  { id: "rearWrist", name: "Rear wrist", boneId: "rearArm", x: 43, y: 0, kind: "socket" as const, color: "#6b96d8", handleBoneId: "rearArm" },
  { id: "frontHip", name: "Front hip", boneId: "frontLeg", x: 0, y: 0, kind: "joint" as const, color: "#78b56b", handleBoneId: "frontLeg" },
  { id: "frontKnee", name: "Front knee", boneId: "frontLeg", x: 30, y: 0, kind: "joint" as const, color: "#78b56b" },
  { id: "frontAnkle", name: "Front ankle", boneId: "frontLeg", x: 58, y: 0, kind: "socket" as const, color: "#78b56b", handleBoneId: "frontLeg" },
  { id: "rearHip", name: "Rear hip", boneId: "rearLeg", x: 0, y: 0, kind: "joint" as const, color: "#78b56b", handleBoneId: "rearLeg" },
  { id: "rearKnee", name: "Rear knee", boneId: "rearLeg", x: 29, y: 0, kind: "joint" as const, color: "#78b56b" },
  { id: "rearAnkle", name: "Rear ankle", boneId: "rearLeg", x: 57, y: 0, kind: "socket" as const, color: "#78b56b", handleBoneId: "rearLeg" },
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

const segmentedPoseLibrary: Record<string, SkeletalPoseDefinition> = {
  bind: {
    id: "bind",
    name: "Bind pose",
    durationMs: 360,
    bones: {
      hips: { x: -2, y: -58 },
      spine: { rotation: -90 },
      head: { rotation: 90 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 0 },
      frontArm: { rotation: 178 },
      rearArm: { rotation: 184 },
      frontLeg: { rotation: 88 },
      rearLeg: { rotation: 94 }
    },
    anchors: {
      frontElbow: { x: 23, y: 0 },
      frontWrist: { x: 45, y: 0 },
      rearElbow: { x: 22, y: 0 },
      rearWrist: { x: 43, y: 0 },
      frontKnee: { x: 30, y: 0 },
      frontAnkle: { x: 58, y: 0 },
      rearKnee: { x: 29, y: 0 },
      rearAnkle: { x: 57, y: 0 }
    }
  },
  idle: {
    id: "idle",
    name: "Idle",
    durationMs: 420,
    bones: {
      hips: { x: -2, y: -58 },
      spine: { rotation: -90 },
      head: { rotation: 90 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 2 },
      frontArm: { rotation: 178 },
      rearArm: { rotation: 184 },
      frontLeg: { rotation: 86 },
      rearLeg: { rotation: 96 }
    },
    anchors: {
      frontElbow: { x: 23, y: 0 },
      frontWrist: { x: 45, y: 0 },
      rearElbow: { x: 22, y: 0 },
      rearWrist: { x: 43, y: 0 },
      frontKnee: { x: 30, y: 0 },
      frontAnkle: { x: 58, y: 0 },
      rearKnee: { x: 29, y: 0 },
      rearAnkle: { x: 57, y: 0 }
    }
  },
  windup: {
    id: "windup",
    name: "Windup",
    durationMs: 180,
    bones: {
      root: { x: 123, y: 177 },
      hips: { x: -3, y: -57 },
      spine: { rotation: -84 },
      head: { rotation: 84 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 10 },
      frontArm: { rotation: 112 },
      rearArm: { rotation: 122 },
      frontLeg: { rotation: 96 },
      rearLeg: { rotation: 104 }
    },
    anchors: {
      frontElbow: { x: 17, y: 10 },
      frontWrist: { x: 24, y: 24 },
      rearElbow: { x: 16, y: 10 },
      rearWrist: { x: 27, y: 23 },
      frontKnee: { x: 27, y: 9 },
      frontAnkle: { x: 45, y: 8 },
      rearKnee: { x: 27, y: 4 },
      rearAnkle: { x: 48, y: 3 }
    }
  },
  strike: {
    id: "strike",
    name: "Strike",
    durationMs: 160,
    bones: {
      root: { x: 142, y: 172 },
      hips: { x: -1, y: -58 },
      spine: { rotation: -100 },
      head: { rotation: 100 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: -12 },
      frontArm: { rotation: 76 },
      rearArm: { rotation: 126 },
      frontLeg: { rotation: 78 },
      rearLeg: { rotation: 110 }
    },
    anchors: {
      frontElbow: { x: 24, y: 1 },
      frontWrist: { x: 53, y: -2 },
      rearElbow: { x: 16, y: 12 },
      rearWrist: { x: 25, y: 24 },
      frontKnee: { x: 31, y: -2 },
      frontAnkle: { x: 57, y: -5 },
      rearKnee: { x: 27, y: 6 },
      rearAnkle: { x: 47, y: 10 }
    }
  },
  recover: {
    id: "recover",
    name: "Recover",
    durationMs: 320,
    bones: {
      root: { x: 132, y: 175 },
      hips: { x: -2, y: -58 },
      spine: { rotation: -94 },
      head: { rotation: 94 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 6 },
      frontArm: { rotation: 94 },
      rearArm: { rotation: 112 },
      frontLeg: { rotation: 84 },
      rearLeg: { rotation: 100 }
    },
    anchors: {
      frontElbow: { x: 22, y: 3 },
      frontWrist: { x: 45, y: 5 },
      rearElbow: { x: 17, y: 8 },
      rearWrist: { x: 31, y: 18 },
      frontKnee: { x: 30, y: 1 },
      frontAnkle: { x: 54, y: -2 },
      rearKnee: { x: 28, y: 0 },
      rearAnkle: { x: 51, y: 1 }
    }
  },
  guard: {
    id: "guard",
    name: "Guard",
    durationMs: 280,
    bones: {
      root: { x: 126, y: 176 },
      spine: { rotation: -92 },
      head: { rotation: 92 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 4 },
      frontArm: { rotation: 82 },
      rearArm: { rotation: 120 },
      frontLeg: { rotation: 88 },
      rearLeg: { rotation: 98 }
    },
    anchors: {
      frontElbow: { x: 20, y: -2 },
      frontWrist: { x: 42, y: -8 },
      rearElbow: { x: 17, y: 9 },
      rearWrist: { x: 30, y: 20 },
      frontKnee: { x: 29, y: 2 },
      frontAnkle: { x: 53, y: -1 },
      rearKnee: { x: 28, y: 0 },
      rearAnkle: { x: 51, y: 2 }
    }
  },
  hurt: {
    id: "hurt",
    name: "Hurt",
    durationMs: 260,
    bones: {
      root: { x: 112, y: 177 },
      spine: { rotation: -72 },
      head: { rotation: 72 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 26 },
      frontArm: { rotation: 130 },
      rearArm: { rotation: 142 },
      frontLeg: { rotation: 100 },
      rearLeg: { rotation: 108 }
    },
    anchors: {
      frontElbow: { x: 17, y: 11 },
      frontWrist: { x: 25, y: 27 },
      rearElbow: { x: 15, y: 12 },
      rearWrist: { x: 24, y: 26 },
      frontKnee: { x: 27, y: 9 },
      frontAnkle: { x: 46, y: 10 },
      rearKnee: { x: 26, y: 7 },
      rearAnkle: { x: 46, y: 9 }
    }
  },
  dodge: {
    id: "dodge",
    name: "Dodge",
    durationMs: 300,
    bones: {
      root: { x: 104, y: 178 },
      hips: { x: -4, y: -54 },
      spine: { rotation: -78 },
      head: { rotation: 78 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 26 },
      frontArm: { rotation: 124 },
      rearArm: { rotation: 132 },
      frontLeg: { rotation: 104 },
      rearLeg: { rotation: 116 }
    },
    anchors: {
      frontElbow: { x: 18, y: 9 },
      frontWrist: { x: 30, y: 22 },
      rearElbow: { x: 17, y: 10 },
      rearWrist: { x: 28, y: 24 },
      frontKnee: { x: 25, y: 11 },
      frontAnkle: { x: 42, y: 15 },
      rearKnee: { x: 26, y: 8 },
      rearAnkle: { x: 45, y: 11 }
    }
  },
  swordReady: {
    id: "swordReady",
    name: "Sword ready",
    durationMs: 320,
    bones: {
      root: { x: 126, y: 176 },
      hips: { x: -2, y: -58 },
      spine: { rotation: -92 },
      head: { rotation: 92 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 4 },
      frontArm: { rotation: 104 },
      rearArm: { rotation: 178 },
      frontLeg: { rotation: 84 },
      rearLeg: { rotation: 102 },
      sword: { x: 42, y: -8, rotation: -14 }
    },
    anchors: {
      frontElbow: { x: 21, y: -4 },
      frontWrist: { x: 42, y: -8 },
      rearElbow: { x: 20, y: 1 },
      rearWrist: { x: 38, y: 3 },
      frontKnee: { x: 30, y: 2 },
      frontAnkle: { x: 54, y: -2 },
      rearKnee: { x: 27, y: 2 },
      rearAnkle: { x: 50, y: 3 }
    },
    bindings: {
      "prop.sword": { opacity: 0.95 }
    }
  },
  swordThrustWindup: {
    id: "swordThrustWindup",
    name: "Sword thrust windup",
    durationMs: 150,
    bones: {
      root: { x: 120, y: 177 },
      hips: { x: -4, y: -57 },
      spine: { rotation: -82 },
      head: { rotation: 82 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 12 },
      frontArm: { rotation: 128 },
      rearArm: { rotation: 190 },
      frontLeg: { rotation: 94 },
      rearLeg: { rotation: 104 },
      sword: { x: 29, y: 18, rotation: -8 }
    },
    anchors: {
      frontElbow: { x: 17, y: 8 },
      frontWrist: { x: 29, y: 18 },
      rearElbow: { x: 20, y: -2 },
      rearWrist: { x: 40, y: -5 },
      frontKnee: { x: 27, y: 7 },
      frontAnkle: { x: 45, y: 6 },
      rearKnee: { x: 27, y: 4 },
      rearAnkle: { x: 49, y: 4 }
    },
    bindings: {
      "prop.sword": { opacity: 0.95 }
    }
  },
  swordThrust: {
    id: "swordThrust",
    name: "Sword thrust",
    durationMs: 130,
    bones: {
      root: { x: 124, y: 172 },
      hips: { x: 0, y: -59 },
      spine: { rotation: -104 },
      head: { rotation: 98 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: -10 },
      frontArm: { rotation: 88 },
      rearArm: { rotation: 172 },
      frontLeg: { rotation: 76 },
      rearLeg: { rotation: 112 },
      sword: { x: 43, y: 0, rotation: -3 }
    },
    anchors: {
      frontElbow: { x: 22, y: 0 },
      frontWrist: { x: 43, y: 0 },
      rearElbow: { x: 21, y: 1 },
      rearWrist: { x: 42, y: 1 },
      frontKnee: { x: 32, y: -3 },
      frontAnkle: { x: 58, y: -6 },
      rearKnee: { x: 28, y: 5 },
      rearAnkle: { x: 47, y: 10 }
    },
    bindings: {
      "prop.sword": { opacity: 1 }
    }
  },
  swordChopWindup: {
    id: "swordChopWindup",
    name: "Sword chop windup",
    durationMs: 170,
    bones: {
      root: { x: 121, y: 176 },
      hips: { x: -3, y: -57 },
      spine: { rotation: -82 },
      head: { rotation: 84 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 12 },
      frontArm: { rotation: 156 },
      rearArm: { rotation: 128 },
      frontLeg: { rotation: 94 },
      rearLeg: { rotation: 104 },
      sword: { x: 31, y: -18, rotation: -58 }
    },
    anchors: {
      frontElbow: { x: 18, y: -10 },
      frontWrist: { x: 31, y: -18 },
      rearElbow: { x: 17, y: 8 },
      rearWrist: { x: 30, y: 18 },
      frontKnee: { x: 28, y: 7 },
      frontAnkle: { x: 46, y: 6 },
      rearKnee: { x: 27, y: 4 },
      rearAnkle: { x: 49, y: 4 }
    },
    bindings: {
      "prop.sword": { opacity: 0.95 }
    }
  },
  swordChop: {
    id: "swordChop",
    name: "Sword chop",
    durationMs: 150,
    bones: {
      root: { x: 126, y: 173 },
      hips: { x: -1, y: -58 },
      spine: { rotation: -104 },
      head: { rotation: 98 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: -8 },
      frontArm: { rotation: 84 },
      rearArm: { rotation: 128 },
      frontLeg: { rotation: 78 },
      rearLeg: { rotation: 110 },
      sword: { x: 36, y: 8, rotation: 38 }
    },
    anchors: {
      frontElbow: { x: 19, y: 3 },
      frontWrist: { x: 36, y: 8 },
      rearElbow: { x: 17, y: 10 },
      rearWrist: { x: 29, y: 21 },
      frontKnee: { x: 31, y: -1 },
      frontAnkle: { x: 56, y: -5 },
      rearKnee: { x: 27, y: 6 },
      rearAnkle: { x: 47, y: 10 }
    },
    bindings: {
      "prop.sword": { opacity: 1 }
    }
  },
  swordLiaoWindup: {
    id: "swordLiaoWindup",
    name: "Sword rising cut windup",
    durationMs: 150,
    bones: {
      root: { x: 122, y: 177 },
      hips: { x: -3, y: -57 },
      spine: { rotation: -84 },
      head: { rotation: 84 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 10 },
      frontArm: { rotation: 112 },
      rearArm: { rotation: 178 },
      frontLeg: { rotation: 96 },
      rearLeg: { rotation: 104 },
      sword: { x: 32, y: 20, rotation: 54 }
    },
    anchors: {
      frontElbow: { x: 18, y: 9 },
      frontWrist: { x: 32, y: 20 },
      rearElbow: { x: 19, y: 0 },
      rearWrist: { x: 39, y: 1 },
      frontKnee: { x: 27, y: 9 },
      frontAnkle: { x: 45, y: 8 },
      rearKnee: { x: 27, y: 4 },
      rearAnkle: { x: 48, y: 3 }
    },
    bindings: {
      "prop.sword": { opacity: 0.95 }
    }
  },
  swordLiao: {
    id: "swordLiao",
    name: "Sword rising cut",
    durationMs: 150,
    bones: {
      root: { x: 126, y: 172 },
      hips: { x: -1, y: -58 },
      spine: { rotation: -96 },
      head: { rotation: 94 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: -6 },
      frontArm: { rotation: 66 },
      rearArm: { rotation: 146 },
      frontLeg: { rotation: 78 },
      rearLeg: { rotation: 112 },
      sword: { x: 36, y: -10, rotation: -36 }
    },
    anchors: {
      frontElbow: { x: 19, y: -5 },
      frontWrist: { x: 36, y: -10 },
      rearElbow: { x: 17, y: 8 },
      rearWrist: { x: 30, y: 17 },
      frontKnee: { x: 31, y: -2 },
      frontAnkle: { x: 56, y: -6 },
      rearKnee: { x: 27, y: 6 },
      rearAnkle: { x: 47, y: 10 }
    },
    bindings: {
      "prop.sword": { opacity: 1 }
    }
  },
  swordRecover: {
    id: "swordRecover",
    name: "Sword recover",
    durationMs: 260,
    bones: {
      root: { x: 130, y: 175 },
      hips: { x: -2, y: -58 },
      spine: { rotation: -94 },
      head: { rotation: 94 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 6 },
      frontArm: { rotation: 96 },
      rearArm: { rotation: 174 },
      frontLeg: { rotation: 84 },
      rearLeg: { rotation: 100 },
      sword: { x: 45, y: 4, rotation: -8 }
    },
    anchors: {
      frontElbow: { x: 22, y: 2 },
      frontWrist: { x: 45, y: 4 },
      rearElbow: { x: 20, y: 0 },
      rearWrist: { x: 39, y: 1 },
      frontKnee: { x: 30, y: 1 },
      frontAnkle: { x: 54, y: -2 },
      rearKnee: { x: 28, y: 0 },
      rearAnkle: { x: 51, y: 1 }
    },
    bindings: {
      "prop.sword": { opacity: 0.95 }
    }
  },
  swordParry: {
    id: "swordParry",
    name: "Sword parry",
    durationMs: 220,
    bones: {
      root: { x: 125, y: 176 },
      hips: { x: -3, y: -58 },
      spine: { rotation: -88 },
      head: { rotation: 88 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 8 },
      frontArm: { rotation: 128 },
      rearArm: { rotation: 184 },
      frontLeg: { rotation: 90 },
      rearLeg: { rotation: 100 },
      sword: { x: 35, y: -12, rotation: -46 }
    },
    anchors: {
      frontElbow: { x: 20, y: -6 },
      frontWrist: { x: 35, y: -12 },
      rearElbow: { x: 20, y: 0 },
      rearWrist: { x: 40, y: 2 },
      frontKnee: { x: 29, y: 3 },
      frontAnkle: { x: 51, y: 1 },
      rearKnee: { x: 28, y: 1 },
      rearAnkle: { x: 50, y: 3 }
    },
    bindings: {
      "prop.sword": { opacity: 1 }
    }
  },
  swordHurt: {
    id: "swordHurt",
    name: "Sword hurt",
    durationMs: 260,
    bones: {
      root: { x: 111, y: 177 },
      hips: { x: -3, y: -57 },
      spine: { rotation: -72 },
      head: { rotation: 72 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 28 },
      frontArm: { rotation: 136 },
      rearArm: { rotation: 150 },
      frontLeg: { rotation: 102 },
      rearLeg: { rotation: 108 },
      sword: { x: 27, y: 25, rotation: 18 }
    },
    anchors: {
      frontElbow: { x: 17, y: 12 },
      frontWrist: { x: 27, y: 25 },
      rearElbow: { x: 15, y: 12 },
      rearWrist: { x: 24, y: 26 },
      frontKnee: { x: 27, y: 9 },
      frontAnkle: { x: 46, y: 10 },
      rearKnee: { x: 26, y: 7 },
      rearAnkle: { x: 46, y: 9 }
    },
    bindings: {
      "prop.sword": { opacity: 0.9 }
    }
  },
  swordDodge: {
    id: "swordDodge",
    name: "Sword dodge",
    durationMs: 280,
    bones: {
      root: { x: 102, y: 178 },
      hips: { x: -5, y: -54 },
      spine: { rotation: -78 },
      head: { rotation: 78 },
      ponytailBase: { rotation: 178 },
      ponytailTail: { rotation: 30 },
      frontArm: { rotation: 124 },
      rearArm: { rotation: 184 },
      frontLeg: { rotation: 106 },
      rearLeg: { rotation: 116 },
      sword: { x: 30, y: 20, rotation: -14 }
    },
    anchors: {
      frontElbow: { x: 18, y: 8 },
      frontWrist: { x: 30, y: 20 },
      rearElbow: { x: 20, y: 0 },
      rearWrist: { x: 38, y: 3 },
      frontKnee: { x: 25, y: 11 },
      frontAnkle: { x: 42, y: 15 },
      rearKnee: { x: 26, y: 8 },
      rearAnkle: { x: 45, y: 11 }
    },
    bindings: {
      "prop.sword": { opacity: 0.95 }
    }
  }
};

const segmentedBaseEntries: AnimationRigEntry[] = profiles.map((profile) => ({
  id: `segmented.v4.${profile}`,
  actionId: "segmented.v4",
  clipId: "segmented-rig-v4",
  label: `segmented rig v4 / ${profile}`,
  profile,
  style: "fist",
  poseId: "idle",
  sprite: null,
  tags: ["segmented", "part-rig", "v4", profile],
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
    id: `segmented.v4.sword.${action.id}.${profile}`,
    actionId: `segmented.v4.sword.${action.id}`,
    clipId: "segmented-rig-v4-sword",
    label: `${action.label} / ${profile}`,
    profile,
    style: "sword" as const,
    poseId: action.poseId,
    sprite: null,
    tags: ["segmented", "part-rig", "v4", profile, "sword", ...action.tags],
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
      "rearHip",
      -18,
      segmentedLegFront,
      179,
      455,
      segmentedLegScale,
      0.68,
      0.09,
      1,
      -94,
      limbMesh(179 * segmentedLegScale, 455 * segmentedLegScale, [
        { anchorId: "rearHip", x: 0.68, y: 0.09, radius: 6.2, sourceRadius: 6.2 },
        { anchorId: "rearKnee", x: 0.5, y: 0.5, radius: 6.4, sourceRadius: 6.4 },
        { anchorId: "rearAnkle", x: 0.29, y: 0.9, radius: 6.8, sourceRadius: 6.8 }
      ])
    ),
    partBinding(
      "part.arm_back",
      "rear arm",
      "rearShoulder",
      -14,
      segmentedArmBack,
      226,
      341,
      segmentedBackArmScale,
      0.31,
      0.12,
      1,
      -14,
      limbMesh(226 * segmentedBackArmScale, 341 * segmentedBackArmScale, [
        { anchorId: "rearShoulder", x: 0.31, y: 0.12, radius: 5.2, sourceRadius: 5.2 },
        { anchorId: "rearElbow", x: 0.53, y: 0.49, radius: 5.3, sourceRadius: 5.3 },
        { anchorId: "rearWrist", x: 0.75, y: 0.83, radius: 5.8, sourceRadius: 5.8 }
      ])
    ),
    partBinding("part.torso", "torso", "spineBase", -8, segmentedTorso, 197, 377, segmentedTorsoScale, 0.5, 0.9, 1, 90),
    partBinding(
      "part.ponytail",
      "ponytail",
      "ponytailRoot",
      -6,
      segmentedPonytail,
      187,
      492,
      segmentedPonytailScale,
      0.72,
      0.15,
      ponytailOpacity,
      0,
      limbMesh(
        187 * segmentedPonytailScale,
        492 * segmentedPonytailScale,
        [
          { anchorId: "ponytailRoot", x: 0.72, y: 0.15, radius: 6.4 },
          { anchorId: "ponytailMid", x: 0.42, y: 0.48, radius: 6.6 },
          { anchorId: "ponytailTip", x: 0.28, y: 0.9, radius: 3.8 }
        ],
        18
      )
    ),
    partBinding("part.head", "head", "head", -4, segmentedHead, 259, 287, segmentedHeadScale, 0.5, 0.88, 1),
    partBinding(
      "part.leg_front",
      "front leg",
      "frontHip",
      4,
      segmentedLegFront,
      179,
      455,
      segmentedLegScale,
      0.68,
      0.09,
      1,
      -88,
      limbMesh(179 * segmentedLegScale, 455 * segmentedLegScale, [
        { anchorId: "frontHip", x: 0.68, y: 0.09, radius: 6.2, sourceRadius: 6.2 },
        { anchorId: "frontKnee", x: 0.5, y: 0.5, radius: 6.4, sourceRadius: 6.4 },
        { anchorId: "frontAnkle", x: 0.29, y: 0.9, radius: 6.8, sourceRadius: 6.8 }
      ])
    ),
    partBinding(
      "part.arm_front",
      "front arm",
      "frontShoulder",
      8,
      segmentedArmFront,
      220,
      347,
      segmentedArmScale,
      0.31,
      0.12,
      1,
      -8,
      limbMesh(220 * segmentedArmScale, 347 * segmentedArmScale, [
        { anchorId: "frontShoulder", x: 0.31, y: 0.12, radius: 5.2, sourceRadius: 5.2 },
        { anchorId: "frontElbow", x: 0.53, y: 0.49, radius: 5.3, sourceRadius: 5.3 },
        { anchorId: "frontWrist", x: 0.75, y: 0.84, radius: 5.8, sourceRadius: 5.8 }
      ])
    ),
    debugBinding("prop.sword", "straight sword", "line", "weaponGrip", 18, 0, 0, 0, swordOpacity, 58, 3.6, "rgba(232, 227, 208, 0.98)"),
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
  keypoints: [
    { anchorId: string; x: number; y: number; radius: number; sourceRadius?: number },
    { anchorId: string; x: number; y: number; radius: number; sourceRadius?: number },
    { anchorId: string; x: number; y: number; radius: number; sourceRadius?: number }
  ],
  segments?: number
): MeshDeformDefinition {
  const deform: MeshDeformDefinition = {
    keypoints: keypoints.map((keypoint) => ({
      anchorId: keypoint.anchorId,
      sourceX: keypoint.x * width,
      sourceY: keypoint.y * height,
      radius: keypoint.radius,
      ...(keypoint.sourceRadius === undefined ? {} : { sourceRadius: keypoint.sourceRadius })
    })) as MeshDeformDefinition["keypoints"]
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
