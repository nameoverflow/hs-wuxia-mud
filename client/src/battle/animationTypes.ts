import type { CombatResult } from "../protocol";

export type BattleSide = "player" | "enemy";
export type VisualProfile = "male" | "female";
export type CombatStyle = "sword" | "fist";

export type BattleTimelineKind = "normal" | "active_skill" | "effect_tick" | "settlement";

export type ActorMotion = "none" | "approach" | "lunge" | "drive" | "focus";

export type TargetReaction = "none" | "hit" | "dodge" | "parry" | "effect";

export interface BattleAnimationFrame {
  frameId: string;
  holdMs: number;
}

export interface ActorVisual {
  kind: "svg";
  actionId: string;
  profile: VisualProfile;
  style: CombatStyle;
  frames: BattleAnimationFrame[];
  keyPoses?: ActionKeyPoses;
}

/** SVG key poses the sampler cuts between; ids refer to resources/scripts/combat_poses/svg-poses.json. */
export interface ActionKeyPoses {
  prepare?: string;
  contact: string;
  finish?: string;
  /** Which point of the contact pose is pinned to choreography.reach/contactY. */
  reachWith?: "hand" | "foot" | "blade";
}

/** Per-action footwork. A presentation lead-in, not server combat time. */
export interface BattleApproach {
  pose: string;
  durationMs: number;
  lift: number;
}

export type VfxArt = "impact" | "slash" | "parry" | "aura" | "thrust" | "rising";

export interface TimelineVfx {
  id: string;
  kind: "trail" | "impact" | "parry" | "aura" | "heal";
  variant: string;
  art: VfxArt;
  side: BattleSide | "center";
}

export interface ActionVfxDefinition {
  kind: TimelineVfx["kind"];
  variant: string;
  art: VfxArt;
  anchor: "actor" | "target" | "center";
}

export interface BattleActionDefinition {
  id: string;
  label: string;
  frameset: "raster-v1";
  style: CombatStyle;
  frames: BattleAnimationFrame[];
  impactFrame: number;
  tags: string[];
  durationMs: number;
  actorMotion: ActorMotion;
  keyPoses?: ActionKeyPoses;
  approach?: BattleApproach;
  targetReaction: Partial<Record<CombatResult, TargetReaction>>;
  vfx: ActionVfxDefinition[];
  choreography: BattleChoreography;
}

/** All markers use the same source clip clock, including the held impact. */
export interface BattleChoreography {
  launchAtMs: number;
  hitStopMs: number;
  recoverAtMs: number;
  restAtMs: number;
  reach: number;
  contactY: number;
  weight: "light" | "heavy" | "quiet";
}

export interface ResolvedBattleTimeline {
  id: number;
  kind: BattleTimelineKind;
  actorSide: BattleSide;
  targetSide: BattleSide;
  durationMs: number;
  impactAtMs: number;
  choreography: BattleChoreography;
  label: string;
  actor: {
    side: BattleSide;
    visual: ActorVisual;
    motion: ActorMotion;
    approach?: BattleApproach;
    actionDelayMs: number;
  };
  target: {
    side: BattleSide;
    visual: ActorVisual;
    reaction: TargetReaction;
  };
  result: CombatResult;
  damage: number | null;
  heal: number | null;
  floatText: string;
  text: string;
  vfx: TimelineVfx[];
}
