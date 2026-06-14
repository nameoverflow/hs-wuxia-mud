import type { CombatResult } from "../protocol";

export type BattleSide = "player" | "enemy";

export type BattleTimelineKind = "normal" | "active_skill" | "effect_tick" | "settlement";

export type ActorMotion = "none" | "approach" | "focus";

export type TargetReaction = "none" | "hit" | "dodge" | "parry" | "effect";

export interface SpriteClip {
  id: string;
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

export interface ResolvedBattleTimeline {
  id: number;
  kind: BattleTimelineKind;
  actorSide: BattleSide;
  targetSide: BattleSide;
  durationMs: number;
  actor: {
    side: BattleSide;
    sprite: string;
    motion: ActorMotion;
  };
  target: {
    side: BattleSide;
    sprite: string;
    reaction: TargetReaction;
  };
  result: CombatResult;
  damage: number | null;
  heal: number | null;
  floatText: string;
  text: string;
  vfx: TimelineVfx[];
}
