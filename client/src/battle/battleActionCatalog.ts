import actionData from "../../../resources/scripts/combat_actions/battle-actions.json";
import { validateClipFrames } from "./animationClip";
import { hasSvgPose } from "./svgPoseLibrary";
import { stageArt } from "./stageAssets";
import type {
  ActorVisual,
  ActionVfxDefinition,
  BattleActionDefinition,
  CombatStyle,
  TargetReaction,
  VisualProfile
} from "./animationTypes";

interface BattleActionManifest {
  schemaVersion: number;
  actions: BattleActionDefinition[];
}

const manifest = actionData as BattleActionManifest;

if (manifest.schemaVersion !== 5) throw new Error(`Unsupported battle action schema ${manifest.schemaVersion}`);
manifest.actions.forEach((action) => {
  validateClipFrames(action.id, action.frames, action.durationMs, action.impactFrame);
  const poses = [
    ...action.frames.map((frame) => frame.frameId),
    ...Object.entries(action.keyPoses || {}).filter(([key]) => key !== "reachWith").map(([, id]) => id as string),
    ...(action.approach ? [action.approach.pose] : []),
    ...(action.poseTrack || []).map((key) => key.pose)
  ];
  for (const pose of poses) if (!hasSvgPose(pose)) throw new Error(`${action.id}: missing SVG pose ${pose}`);
  for (const vfx of action.vfx || []) if (!(vfx.art in stageArt)) throw new Error(`${action.id}: unknown vfx art ${vfx.art}`);
});

export const battleActions: Record<string, BattleActionDefinition> = Object.fromEntries(
  manifest.actions.map((action) => [action.id, normalizeAction(action)])
);

export function battleActionFor(actionId: string | null | undefined, fallbackStyle: CombatStyle): BattleActionDefinition {
  if (actionId) {
    const action = battleActions[actionId];
    if (!action) throw new Error(`Unknown battle action: ${actionId}`);
    return action;
  }
  return battleActions[defaultActionIdForStyle(fallbackStyle)];
}

export function visualForBattleAction(actionId: string, profile: VisualProfile, fallbackStyle: CombatStyle): ActorVisual {
  const action = battleActionFor(actionId, fallbackStyle);
  return {
    kind: "svg",
    actionId: action.id,
    profile,
    style: action.style,
    frames: action.frames,
    keyPoses: action.keyPoses
  };
}

export function idleVisualForStyle(style: CombatStyle, profile: VisualProfile): ActorVisual {
  return visualForBattleAction(idleActionIdForStyle(style), profile, style);
}

export function reactionVisualFor(reaction: Exclude<TargetReaction, "none">, profile: VisualProfile, style: CombatStyle): ActorVisual {
  return visualForBattleAction(reactionActionIdForStyle(reaction, style), profile, style);
}

export function visualProfileFromGender(gender: string | null | undefined): VisualProfile {
  return gender === "female" ? "female" : "male";
}

export function combatStyleFromSnapshot(style: string | null | undefined): CombatStyle {
  return style === "sword" ? "sword" : "fist";
}

export function defaultActionIdForStyle(style: CombatStyle) {
  return style === "sword" ? "rig.sword.chop_a" : "rig.fist.punch_a";
}

export function idleActionIdForStyle(style: CombatStyle) {
  return style === "sword" ? "rig.sword.idle" : "rig.fist.idle";
}

export function reactionActionIdForStyle(reaction: Exclude<TargetReaction, "none">, style: CombatStyle) {
  if (reaction === "hit") return style === "sword" ? "rig.sword.hurt" : "rig.fist.hurt";
  if (reaction === "dodge") return style === "sword" ? "rig.sword.dodge" : "rig.fist.dodge";
  if (reaction === "parry") return style === "sword" ? "rig.sword.parry" : "rig.fist.parry";
  return idleActionIdForStyle(style);
}

function normalizeAction(action: BattleActionDefinition): BattleActionDefinition {
  return {
    ...action,
    tags: action.tags || [],
    targetReaction: action.targetReaction || {},
    vfx: (action.vfx || []) as ActionVfxDefinition[]
  };
}
