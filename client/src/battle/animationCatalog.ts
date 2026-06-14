import type { BattleActionDefinition, SpriteClip } from "./animationTypes";
import idleSprite from "../assets/battle/actors/sword/idle.png";
import stabSprite from "../assets/battle/actors/sword/attack/stab-a.png";
import slashSprite from "../assets/battle/actors/sword/attack/slash-a.png";
import uppercutSprite from "../assets/battle/actors/sword/attack/uppercut-a.png";
import hurtSprite from "../assets/battle/actors/common/hurt.png";
import dodgeSprite from "../assets/battle/actors/common/dodge.png";
import parrySprite from "../assets/battle/actors/common/parry.png";

export const actorIdleSprite = idleSprite;

export const reactionSprites = {
  hit: hurtSprite,
  dodge: dodgeSprite,
  parry: parrySprite,
  effect: idleSprite
};

export const spriteClips: Record<string, SpriteClip> = {
  "actor.sword.idle": { id: "actor.sword.idle", sprite: idleSprite },
  "actor.sword.stab_a": { id: "actor.sword.stab_a", sprite: stabSprite },
  "actor.sword.slash_a": { id: "actor.sword.slash_a", sprite: slashSprite },
  "actor.sword.uppercut_a": { id: "actor.sword.uppercut_a", sprite: uppercutSprite }
};

export const battleActions: Record<string, BattleActionDefinition> = {
  "sword.stab_a": {
    id: "sword.stab_a",
    clipId: "actor.sword.stab_a",
    tags: ["sword", "stab", "thrust", "heavy", "fist", "strike"],
    durationMs: 760,
    actorMotion: "approach",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "stab-line", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "sword.slash_a": {
    id: "sword.slash_a",
    clipId: "actor.sword.slash_a",
    tags: ["sword", "slash", "chop", "cut", "fist", "heavy"],
    durationMs: 840,
    actorMotion: "approach",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "slash-arc", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "sword.uppercut_a": {
    id: "sword.uppercut_a",
    clipId: "actor.sword.uppercut_a",
    tags: ["sword", "uppercut", "lift", "kick", "fist"],
    durationMs: 900,
    actorMotion: "approach",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "uppercut-arc", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "skill.self_focus.guard": {
    id: "skill.self_focus.guard",
    clipId: "actor.sword.idle",
    tags: ["buff", "stance", "self"],
    durationMs: 720,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "aura", variant: "guard-ring", anchor: "actor" }]
  },
  "skill.self_focus.palm": {
    id: "skill.self_focus.palm",
    clipId: "actor.sword.idle",
    tags: ["heal", "self"],
    durationMs: 720,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "heal", variant: "heal-pulse", anchor: "target" }]
  },
  "effect.dot": {
    id: "effect.dot",
    clipId: "actor.sword.idle",
    tags: ["effect", "dot"],
    durationMs: 560,
    actorMotion: "none",
    targetReaction: { hit: "hit", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "impact", variant: "dot-spark", anchor: "target" }]
  },
  "effect.hot": {
    id: "effect.hot",
    clipId: "actor.sword.idle",
    tags: ["effect", "hot"],
    durationMs: 560,
    actorMotion: "none",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "heal", variant: "heal-pulse", anchor: "target" }]
  }
};

export const actionPools: Record<string, string[]> = {
  "weapon.sword.basic": ["sword.stab_a", "sword.slash_a", "sword.uppercut_a"],
  "weapon.fist.basic": ["sword.stab_a", "sword.slash_a", "sword.uppercut_a"],
  "skill.self_focus": ["skill.self_focus.guard", "skill.self_focus.palm"],
  "effect.tick": ["effect.dot", "effect.hot"]
};

export function spriteForClip(clipId: string) {
  return spriteClips[clipId]?.sprite || actorIdleSprite;
}
