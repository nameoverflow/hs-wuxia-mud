import type { CombatResult } from "../protocol";

export type BattleSide = "player" | "enemy";
export type VisualProfile = "male" | "female";
export type CombatStyle = "sword" | "fist";

export type BattleTimelineKind = "normal" | "active_skill" | "effect_tick" | "settlement";

export type ActorMotion = "none" | "approach" | "lunge" | "drive" | "focus";

export type TargetReaction = "none" | "hit" | "dodge" | "parry" | "effect";

export interface SpriteClip {
  id: string;
  sprites: Record<VisualProfile, string>;
}

export interface ActorVisual {
  kind: "sprite";
  sprite: string;
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
  clipId: string;
  tags: string[];
  durationMs: number;
  actorMotion: ActorMotion;
  targetReaction: Partial<Record<CombatResult, TargetReaction>>;
  vfx: ActionVfxDefinition[];
}

export interface ActionPoolCandidate {
  id: string;
  weight?: number;
  tags?: string[];
}

export type ActionPoolEntry = string | ActionPoolCandidate;

export interface ActionPoolVariant {
  mode?: "append" | "replace";
  actions: ActionPoolEntry[];
}

export interface ActionPoolDefinition {
  id: string;
  fallbackPool?: string;
  actions: ActionPoolEntry[];
  styles?: Partial<Record<CombatStyle, ActionPoolVariant>>;
  profiles?: Partial<Record<VisualProfile, ActionPoolVariant>>;
  styleProfiles?: Partial<Record<CombatStyle, Partial<Record<VisualProfile, ActionPoolVariant>>>>;
}

export interface ActionVariantDefinition {
  styles?: Partial<Record<CombatStyle, string>>;
  profiles?: Partial<Record<VisualProfile, string>>;
  styleProfiles?: Partial<Record<CombatStyle, Partial<Record<VisualProfile, string>>>>;
}

export interface ResolvedBattleTimeline {
  id: number;
  kind: BattleTimelineKind;
  actorSide: BattleSide;
  targetSide: BattleSide;
  durationMs: number;
  actor: {
    side: BattleSide;
    sprite: string;
    visual: ActorVisual;
    motion: ActorMotion;
  };
  target: {
    side: BattleSide;
    sprite: string;
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
