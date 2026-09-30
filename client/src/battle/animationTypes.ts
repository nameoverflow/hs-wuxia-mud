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

/** One contact inside an action, on the source clip clock. Omitted → a single hit at impactFrame. */
export interface ActionHitDefinition {
  atMs: number;
  hitStopMs: number;
  /** Relative portion of the event's damage/heal; defaults to an even split. */
  share?: number;
}

export type ReachPoint = NonNullable<ActionKeyPoses["reachWith"]>;

/** A hard-cut pose key on the source clip clock (after the approach). */
export interface PoseKeyDefinition {
  atMs: number;
  pose: string;
  /** Pin this point of the pose to the contact (reach/contactY); omitted → pose as authored. */
  pin?: ReachPoint;
  reach?: number;
  contactY?: number;
}

export interface ResolvedHit {
  atMs: number;
  hitStopMs: number;
  damage: number | null;
  heal: number | null;
  floatText: string;
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
  hits?: ActionHitDefinition[];
  poseTrack?: PoseKeyDefinition[];
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
  /** First hit; kept as a convenience for single-contact consumers. */
  impactAtMs: number;
  /** Every contact in timeline time, sorted. Holds never overlap the next hit. */
  hits: ResolvedHit[];
  choreography: BattleChoreography;
  label: string;
  actor: {
    side: BattleSide;
    visual: ActorVisual;
    motion: ActorMotion;
    approach?: BattleApproach;
    /** Hard-cut pose keys in timeline time; empty → pose from the current clip frame. */
    poseKeys: PoseKeyDefinition[];
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
