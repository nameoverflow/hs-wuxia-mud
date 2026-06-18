import rigActionData from "../../../resources/scripts/combat_actions/rig-actions.json";
import type { AnimationRigEntry } from "./skeletal/types";
import type {
  ActorVisual,
  ActionVfxDefinition,
  BattleActionDefinition,
  CombatStyle,
  TargetReaction,
  VisualProfile
} from "./animationTypes";

interface RigActionManifest {
  schemaVersion: number;
  actions: BattleActionDefinition[];
}

const manifest = rigActionData as RigActionManifest;
const profiles: VisualProfile[] = ["male", "female"];

export const rigActions: Record<string, BattleActionDefinition> = Object.fromEntries(
  manifest.actions.map((action) => [action.id, normalizeAction(action)])
);

export const rigActionEntries: AnimationRigEntry[] = manifest.actions.flatMap((action) =>
  profiles.map((profile) => ({
    id: rigEntryId(action.id, profile),
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

export function rigEntryId(actionId: string, profile: VisualProfile) {
  return `${actionId}.${profile}`;
}

export function rigActionFor(actionId: string | null | undefined, fallbackStyle: CombatStyle): BattleActionDefinition {
  if (actionId && rigActions[actionId]) return rigActions[actionId];
  return rigActions[defaultActionIdForStyle(fallbackStyle)];
}

export function visualForRigAction(actionId: string, profile: VisualProfile, fallbackStyle: CombatStyle): ActorVisual {
  const action = rigActionFor(actionId, fallbackStyle);
  return {
    kind: "rig",
    actionId: action.id,
    entryId: rigEntryId(action.id, profile),
    profile,
    style: action.style,
    poseId: action.poseId,
    sequence: action.sequence
  };
}

export function idleVisualForStyle(style: CombatStyle, profile: VisualProfile): ActorVisual {
  return visualForRigAction(idleActionIdForStyle(style), profile, style);
}

export function reactionVisualFor(reaction: Exclude<TargetReaction, "none">, profile: VisualProfile, style: CombatStyle): ActorVisual {
  return visualForRigAction(reactionActionIdForStyle(reaction, style), profile, style);
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
    sequence: action.sequence?.length ? action.sequence : [action.poseId],
    tags: action.tags || [],
    targetReaction: action.targetReaction || {},
    vfx: (action.vfx || []) as ActionVfxDefinition[]
  };
}
