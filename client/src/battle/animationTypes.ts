import type { CombatResult } from "../protocol";

export type BattleSide = "player" | "enemy";
export type VisualProfile = "male" | "female";
export type CombatStyle = "sword" | "fist";

export type BattleTimelineKind = "normal" | "active_skill" | "effect_tick" | "settlement";

export type ActorMotion = "none" | "approach" | "lunge" | "drive" | "focus";

export type TargetReaction = "none" | "hit" | "dodge" | "parry" | "effect";

export interface ActorVisual {
  kind: "rig";
  actionId: string;
  entryId: string;
  profile: VisualProfile;
  style: CombatStyle;
  poseId: string;
  sequence: string[];
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
  rig: "segmented-v12";
  style: CombatStyle;
  poseId: string;
  sequence: string[];
  tags: string[];
  durationMs: number;
  actorMotion: ActorMotion;
  targetReaction: Partial<Record<CombatResult, TargetReaction>>;
  vfx: ActionVfxDefinition[];
}

export interface ResolvedBattleTimeline {
  id: number;
  kind: BattleTimelineKind;
  actorSide: BattleSide;
  targetSide: BattleSide;
  durationMs: number;
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
