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
}

export interface TimelineVfx {
  id: string;
  kind: "trail" | "impact" | "parry" | "aura" | "heal";
  variant: string;
  side: BattleSide | "center";
}

export interface ActionVfxDefinition {
  kind: TimelineVfx["kind"];
  variant: string;
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
