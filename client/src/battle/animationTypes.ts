import type { CombatResult } from "../protocol";

export type BattleSide = "player" | "enemy";
export type VisualProfile = "male" | "female";
export type CombatStyle = "sword" | "fist";

export type BattleTimelineKind = "normal" | "active_skill" | "effect_tick" | "settlement";

/** approach/lunge/drive travel to the target; ranged strikes from home; focus holds a stance. */
export type ActorMotion = "none" | "approach" | "lunge" | "drive" | "focus" | "ranged";

export const TRAVEL_MOTIONS: ActorMotion[] = ["approach", "lunge", "drive"];

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

/** SVG key poses the sampler cuts between; ids refer to resources/scripts/combat_presentation/svg-poses.json. */
export interface ActionKeyPoses {
  prepare?: string;
  contact: string;
  /** Follow-through for actions that stay in place; travelling actions retreat instead. */
  finish?: string;
  /** Pose held while sliding back home; defaults to "retreat". */
  retreat?: string;
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
  /** Per-hit staging overrides (camera kick, reaction push, …) on top of the action's profile. */
  staging?: StagingOverride;
}

export interface ReactionStaging {
  /** Defender displacement away from the attacker, source px. */
  push: number;
  /** Defender lean in degrees (hit only). */
  tilt?: number;
  /** Defender lift, source px upward (hit only). */
  lift?: number;
  /** Dodge/parry start this long before contact. */
  leadMs?: number;
  /** Dodge/parry ease-in length; hits always snap. */
  onsetMs?: number;
  /** Dodge afterimage opacity. */
  ghost?: number;
  /** Parry: how far short of the defender a pinned weapon stops. */
  standoff?: number;
  /** Override the defender pose (svg-poses id) for this reaction; wins over beats. */
  pose?: string;
  /** Dodge hop height (px) across the evasion. */
  hop?: number;
  /** Hit: fraction of push applied at contact; the rest slides in over the next 200ms as a stagger. */
  snap?: number;
}

/**
 * Every tunable of the stage director. Presets live in combat_presentation/staging.json
 * (light/heavy/quiet by choreography.weight); actions and single hits override parts.
 */
export interface StagingProfile {
  /** 0..1 strike weight: stage stamp, ink burst scale, arc tilt. */
  force: number;
  camera: { kick: number; rebound: number; lift: number; zoom: number };
  shade: number;
  tilt: number;
  flash: number;
  fallMs: number;
  trail: { leadMs: number; fadeMs: number };
  reactions: { hit: ReactionStaging; dodge: ReactionStaging; parry: ReactionStaging };
}

export type StagingOverride = {
  [K in keyof StagingProfile]?: StagingProfile[K] extends object ? { [P in keyof StagingProfile[K]]?: Partial<StagingProfile[K][P]> | StagingProfile[K][P] } : StagingProfile[K];
};

/** Additive actor root offset (x forward, y down) and lean, source clip clock. */
export interface OffsetKeyDefinition {
  atMs: number;
  x?: number;
  y?: number;
  angle?: number;
  /** How the value travels from the previous key: cut (hold then jump), linear, or out (fast then settle). */
  ease?: "cut" | "linear" | "out";
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
  result: CombatResult;
  reaction: TargetReaction;
  /** Defender clip for this hit's reaction. */
  targetVisual: ActorVisual;
  staging: StagingProfile;
}

export type VfxArt = "impact" | "slash" | "parry" | "aura" | "thrust" | "rising";

/**
 * trail/impact/parry/aura/heal are the built-in layers driven by the director's scalars;
 * sprite is a free ink sprite with its own timing, anchors and motion; custom calls a registered sampler.
 */
export type VfxKind = "trail" | "impact" | "parry" | "aura" | "heal" | "sprite" | "custom";

/** Where a sprite sits: the contact point, a figure's chest, the stage centre, or a live limb of the attacker. */
export type VfxAnchorRef = "contact" | "actor" | "target" | "center" | "actor.hand" | "actor.foot" | "actor.blade";

export interface SpriteVfxOptions {
  /** Start on the source clip clock; omitted → relative to hits[hit].atMs + offsetMs. */
  atMs?: number;
  hit?: number;
  offsetMs?: number;
  durationMs?: number;
  from?: VfxAnchorRef;
  /** Travel from → to over the sprite's life. */
  to?: VfxAnchorRef;
  /** Rendered box size in source px. */
  size?: number;
  scale?: [number, number];
  rotate?: number;
  /** Extra rotation over the sprite's life, degrees. */
  spin?: number;
  /** Mirror the artwork against the attack direction, e.g. so a crescent flies convex side first. */
  mirror?: boolean;
  opacity?: number;
  fadeInMs?: number;
  fadeOutMs?: number;
  /** Only when the anchored hit ends in one of these results. */
  results?: CombatResult[];
  /** custom: registered sampler name and its parameters. */
  effect?: string;
  params?: Record<string, unknown>;
}

export interface TimelineVfx extends SpriteVfxOptions {
  id: string;
  kind: VfxKind;
  variant: string;
  art: VfxArt;
  side: BattleSide | "center";
  /** Resolved sprite lifetime in timeline time. */
  startMs: number;
  endMs: number;
}

export interface ActionVfxDefinition extends SpriteVfxOptions {
  kind: VfxKind;
  variant: string;
  art: VfxArt;
  anchor: "actor" | "target" | "center";
}

export interface BattleActionDefinition {
  id: string;
  label: string;
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
  offsetTrack?: OffsetKeyDefinition[];
  staging?: StagingOverride;
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
  /** Action-level staging (camera zoom, stage tilt/shade, trail); per-hit feedback reads hits[i].staging. */
  staging: StagingProfile;
  label: string;
  actor: {
    side: BattleSide;
    visual: ActorVisual;
    motion: ActorMotion;
    approach?: BattleApproach;
    /** Hard-cut pose keys in timeline time; empty → pose from the current clip frame. */
    poseKeys: PoseKeyDefinition[];
    /** Root offset keys in timeline time. */
    offsetKeys: OffsetKeyDefinition[];
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
