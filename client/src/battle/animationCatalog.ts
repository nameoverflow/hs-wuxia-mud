import type {
  ActorVisual,
  ActionPoolCandidate,
  ActionPoolDefinition,
  ActionPoolEntry,
  ActionPoolVariant,
  ActionVariantDefinition,
  BattleActionDefinition,
  CombatStyle,
  SpriteClip,
  TargetReaction,
  VisualProfile
} from "./animationTypes";

import commonFemaleDodge from "../assets/battle/actors/common/female/dodge.png";
import commonFemaleHurt from "../assets/battle/actors/common/female/hurt.png";
import commonFemaleParry from "../assets/battle/actors/common/female/parry.png";
import commonMaleDodge from "../assets/battle/actors/common/male/dodge.png";
import commonMaleHurt from "../assets/battle/actors/common/male/hurt.png";
import commonMaleParry from "../assets/battle/actors/common/male/parry.png";
import fistFemaleGuard from "../assets/battle/actors/fist/female/guard.png";
import fistFemaleHealingPalm from "../assets/battle/actors/fist/female/healing_palm.png";
import fistFemaleHeavy from "../assets/battle/actors/fist/female/heavy.png";
import fistFemaleIdle from "../assets/battle/actors/fist/female/idle.png";
import fistFemaleKick from "../assets/battle/actors/fist/female/kick.png";
import fistFemalePunch from "../assets/battle/actors/fist/female/punch.png";
import fistMaleGuard from "../assets/battle/actors/fist/male/guard.png";
import fistMaleHealingPalm from "../assets/battle/actors/fist/male/healing_palm.png";
import fistMaleHeavy from "../assets/battle/actors/fist/male/heavy.png";
import fistMaleIdle from "../assets/battle/actors/fist/male/idle.png";
import fistMaleKick from "../assets/battle/actors/fist/male/kick.png";
import fistMalePunch from "../assets/battle/actors/fist/male/punch.png";
import swordFemaleGuard from "../assets/battle/actors/sword/female/guard.png";
import swordFemaleIdle from "../assets/battle/actors/sword/female/idle.png";
import swordFemaleSlash from "../assets/battle/actors/sword/female/slash.png";
import swordFemaleStab from "../assets/battle/actors/sword/female/stab.png";
import swordFemaleUppercut from "../assets/battle/actors/sword/female/uppercut.png";
import swordMaleGuard from "../assets/battle/actors/sword/male/guard.png";
import swordMaleIdle from "../assets/battle/actors/sword/male/idle.png";
import swordMaleSlash from "../assets/battle/actors/sword/male/slash.png";
import swordMaleStab from "../assets/battle/actors/sword/male/stab.png";
import swordMaleUppercut from "../assets/battle/actors/sword/male/uppercut.png";

const sprites = (male: string, female: string): SpriteClip["sprites"] => ({ male, female });
const candidate = (id: string, weight = 1, tags: string[] = []): ActionPoolCandidate => ({ id, weight, tags });
const replaceWith = (actions: ActionPoolEntry[]): ActionPoolVariant => ({ mode: "replace", actions });

export const spriteClips: Record<string, SpriteClip> = {
  "actor.sword.idle": { id: "actor.sword.idle", sprites: sprites(swordMaleIdle, swordFemaleIdle) },
  "actor.sword.stab_a": { id: "actor.sword.stab_a", sprites: sprites(swordMaleStab, swordFemaleStab) },
  "actor.sword.slash_a": { id: "actor.sword.slash_a", sprites: sprites(swordMaleSlash, swordFemaleSlash) },
  "actor.sword.uppercut_a": { id: "actor.sword.uppercut_a", sprites: sprites(swordMaleUppercut, swordFemaleUppercut) },
  "actor.sword.guard": { id: "actor.sword.guard", sprites: sprites(swordMaleGuard, swordFemaleGuard) },
  "actor.fist.idle": { id: "actor.fist.idle", sprites: sprites(fistMaleIdle, fistFemaleIdle) },
  "actor.fist.punch": { id: "actor.fist.punch", sprites: sprites(fistMalePunch, fistFemalePunch) },
  "actor.fist.heavy": { id: "actor.fist.heavy", sprites: sprites(fistMaleHeavy, fistFemaleHeavy) },
  "actor.fist.kick": { id: "actor.fist.kick", sprites: sprites(fistMaleKick, fistFemaleKick) },
  "actor.fist.guard": { id: "actor.fist.guard", sprites: sprites(fistMaleGuard, fistFemaleGuard) },
  "actor.fist.healing_palm": { id: "actor.fist.healing_palm", sprites: sprites(fistMaleHealingPalm, fistFemaleHealingPalm) },
  "actor.common.hurt": { id: "actor.common.hurt", sprites: sprites(commonMaleHurt, commonFemaleHurt) },
  "actor.common.dodge": { id: "actor.common.dodge", sprites: sprites(commonMaleDodge, commonFemaleDodge) },
  "actor.common.parry": { id: "actor.common.parry", sprites: sprites(commonMaleParry, commonFemaleParry) }
};

const reactionClips: Record<Exclude<TargetReaction, "none">, string> = {
  hit: "actor.common.hurt",
  dodge: "actor.common.dodge",
  parry: "actor.common.parry",
  effect: ""
};

export const battleActions: Record<string, BattleActionDefinition> = {
  "sword.stab_a": {
    id: "sword.stab_a",
    clipId: "actor.sword.stab_a",
    tags: ["sword", "stab", "thrust", "heavy"],
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
    tags: ["sword", "slash", "chop", "cut", "heavy"],
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
    tags: ["sword", "uppercut", "lift"],
    durationMs: 900,
    actorMotion: "approach",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "uppercut-arc", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "sword.male.stab_drive": {
    id: "sword.male.stab_drive",
    clipId: "actor.sword.stab_a",
    tags: ["sword", "stab", "thrust", "heavy", "male", "drive"],
    durationMs: 820,
    actorMotion: "drive",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "stab-line", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "sword.male.slash_drive": {
    id: "sword.male.slash_drive",
    clipId: "actor.sword.slash_a",
    tags: ["sword", "slash", "chop", "cut", "heavy", "male", "drive"],
    durationMs: 920,
    actorMotion: "drive",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "slash-arc", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "sword.male.uppercut_drive": {
    id: "sword.male.uppercut_drive",
    clipId: "actor.sword.uppercut_a",
    tags: ["sword", "uppercut", "lift", "heavy", "male", "drive"],
    durationMs: 960,
    actorMotion: "drive",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "uppercut-arc", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "sword.female.stab_lunge": {
    id: "sword.female.stab_lunge",
    clipId: "actor.sword.stab_a",
    tags: ["sword", "stab", "thrust", "quick", "female", "lunge"],
    durationMs: 700,
    actorMotion: "lunge",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "stab-line", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "sword.female.slash_lunge": {
    id: "sword.female.slash_lunge",
    clipId: "actor.sword.slash_a",
    tags: ["sword", "slash", "cut", "quick", "female", "lunge"],
    durationMs: 760,
    actorMotion: "lunge",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "slash-arc", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "sword.female.uppercut_lunge": {
    id: "sword.female.uppercut_lunge",
    clipId: "actor.sword.uppercut_a",
    tags: ["sword", "uppercut", "lift", "quick", "female", "lunge"],
    durationMs: 780,
    actorMotion: "lunge",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "uppercut-arc", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "fist.punch": {
    id: "fist.punch",
    clipId: "actor.fist.punch",
    tags: ["fist", "strike", "punch"],
    durationMs: 720,
    actorMotion: "approach",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "stab-line", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "fist.heavy": {
    id: "fist.heavy",
    clipId: "actor.fist.heavy",
    tags: ["fist", "heavy", "strike"],
    durationMs: 820,
    actorMotion: "approach",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "stab-line", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "fist.kick": {
    id: "fist.kick",
    clipId: "actor.fist.kick",
    tags: ["fist", "kick", "uppercut", "lift"],
    durationMs: 860,
    actorMotion: "approach",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "uppercut-arc", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "fist.male.punch_drive": {
    id: "fist.male.punch_drive",
    clipId: "actor.fist.punch",
    tags: ["fist", "strike", "punch", "male", "drive"],
    durationMs: 780,
    actorMotion: "drive",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "stab-line", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "fist.male.heavy_drive": {
    id: "fist.male.heavy_drive",
    clipId: "actor.fist.heavy",
    tags: ["fist", "heavy", "strike", "male", "drive"],
    durationMs: 900,
    actorMotion: "drive",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "stab-line", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "fist.male.kick_drive": {
    id: "fist.male.kick_drive",
    clipId: "actor.fist.kick",
    tags: ["fist", "kick", "uppercut", "lift", "male", "drive"],
    durationMs: 900,
    actorMotion: "drive",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "uppercut-arc", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "fist.female.punch_lunge": {
    id: "fist.female.punch_lunge",
    clipId: "actor.fist.punch",
    tags: ["fist", "strike", "punch", "quick", "female", "lunge"],
    durationMs: 680,
    actorMotion: "lunge",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "stab-line", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "fist.female.heavy_lunge": {
    id: "fist.female.heavy_lunge",
    clipId: "actor.fist.heavy",
    tags: ["fist", "heavy", "strike", "quick", "female", "lunge"],
    durationMs: 760,
    actorMotion: "lunge",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "stab-line", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "fist.female.kick_lunge": {
    id: "fist.female.kick_lunge",
    clipId: "actor.fist.kick",
    tags: ["fist", "kick", "uppercut", "lift", "quick", "female", "lunge"],
    durationMs: 720,
    actorMotion: "lunge",
    targetReaction: { hit: "hit", dodge: "dodge", parry: "parry", effect: "effect" },
    vfx: [
      { kind: "trail", variant: "uppercut-arc", anchor: "actor" },
      { kind: "impact", variant: "hit-spark", anchor: "target" },
      { kind: "parry", variant: "parry-arc", anchor: "target" }
    ]
  },
  "skill.sword_focus.guard": {
    id: "skill.sword_focus.guard",
    clipId: "actor.sword.guard",
    tags: ["sword", "buff", "stance", "self"],
    durationMs: 720,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "aura", variant: "guard-ring", anchor: "actor" }]
  },
  "skill.fist_focus.guard": {
    id: "skill.fist_focus.guard",
    clipId: "actor.fist.guard",
    tags: ["fist", "buff", "stance", "self"],
    durationMs: 720,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "aura", variant: "guard-ring", anchor: "actor" }]
  },
  "skill.fist_focus.palm": {
    id: "skill.fist_focus.palm",
    clipId: "actor.fist.healing_palm",
    tags: ["fist", "heal", "self"],
    durationMs: 720,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "heal", variant: "heal-pulse", anchor: "target" }]
  },
  "skill.sword_focus.male_guard": {
    id: "skill.sword_focus.male_guard",
    clipId: "actor.sword.guard",
    tags: ["sword", "buff", "stance", "self", "male", "drive"],
    durationMs: 780,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "aura", variant: "guard-ring", anchor: "actor" }]
  },
  "skill.sword_focus.female_guard": {
    id: "skill.sword_focus.female_guard",
    clipId: "actor.sword.guard",
    tags: ["sword", "buff", "stance", "self", "female", "quick"],
    durationMs: 660,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "aura", variant: "guard-ring", anchor: "actor" }]
  },
  "skill.fist_focus.male_guard": {
    id: "skill.fist_focus.male_guard",
    clipId: "actor.fist.guard",
    tags: ["fist", "buff", "stance", "self", "male", "drive"],
    durationMs: 780,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "aura", variant: "guard-ring", anchor: "actor" }]
  },
  "skill.fist_focus.female_guard": {
    id: "skill.fist_focus.female_guard",
    clipId: "actor.fist.guard",
    tags: ["fist", "buff", "stance", "self", "female", "quick"],
    durationMs: 660,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "aura", variant: "guard-ring", anchor: "actor" }]
  },
  "skill.fist_focus.male_palm": {
    id: "skill.fist_focus.male_palm",
    clipId: "actor.fist.healing_palm",
    tags: ["fist", "heal", "self", "male", "drive"],
    durationMs: 760,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "heal", variant: "heal-pulse", anchor: "target" }]
  },
  "skill.fist_focus.female_palm": {
    id: "skill.fist_focus.female_palm",
    clipId: "actor.fist.healing_palm",
    tags: ["fist", "heal", "self", "female", "quick"],
    durationMs: 640,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "heal", variant: "heal-pulse", anchor: "target" }]
  },
  "skill.self_focus.guard": {
    id: "skill.self_focus.guard",
    clipId: "actor.fist.guard",
    tags: ["buff", "stance", "self"],
    durationMs: 720,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "aura", variant: "guard-ring", anchor: "actor" }]
  },
  "skill.self_focus.palm": {
    id: "skill.self_focus.palm",
    clipId: "actor.fist.healing_palm",
    tags: ["heal", "self"],
    durationMs: 720,
    actorMotion: "focus",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "heal", variant: "heal-pulse", anchor: "target" }]
  },
  "effect.dot": {
    id: "effect.dot",
    clipId: "",
    tags: ["effect", "dot"],
    durationMs: 560,
    actorMotion: "none",
    targetReaction: { hit: "hit", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "impact", variant: "dot-spark", anchor: "target" }]
  },
  "effect.hot": {
    id: "effect.hot",
    clipId: "",
    tags: ["effect", "hot"],
    durationMs: 560,
    actorMotion: "none",
    targetReaction: { hit: "effect", dodge: "effect", parry: "effect", effect: "effect" },
    vfx: [{ kind: "heal", variant: "heal-pulse", anchor: "target" }]
  }
};

export const actionVariants: Record<string, ActionVariantDefinition> = {
  "sword.stab_a": {
    profiles: { male: "sword.male.stab_drive", female: "sword.female.stab_lunge" }
  },
  "sword.slash_a": {
    profiles: { male: "sword.male.slash_drive", female: "sword.female.slash_lunge" }
  },
  "sword.uppercut_a": {
    profiles: { male: "sword.male.uppercut_drive", female: "sword.female.uppercut_lunge" }
  },
  "fist.punch": {
    profiles: { male: "fist.male.punch_drive", female: "fist.female.punch_lunge" }
  },
  "fist.heavy": {
    profiles: { male: "fist.male.heavy_drive", female: "fist.female.heavy_lunge" }
  },
  "fist.kick": {
    profiles: { male: "fist.male.kick_drive", female: "fist.female.kick_lunge" }
  },
  "skill.sword_focus.guard": {
    profiles: { male: "skill.sword_focus.male_guard", female: "skill.sword_focus.female_guard" }
  },
  "skill.fist_focus.guard": {
    profiles: { male: "skill.fist_focus.male_guard", female: "skill.fist_focus.female_guard" }
  },
  "skill.fist_focus.palm": {
    profiles: { male: "skill.fist_focus.male_palm", female: "skill.fist_focus.female_palm" }
  },
  "skill.self_focus.guard": {
    styleProfiles: {
      sword: { male: "skill.sword_focus.male_guard", female: "skill.sword_focus.female_guard" },
      fist: { male: "skill.fist_focus.male_guard", female: "skill.fist_focus.female_guard" }
    }
  },
  "skill.self_focus.palm": {
    profiles: { male: "skill.fist_focus.male_palm", female: "skill.fist_focus.female_palm" }
  }
};

export const actionPools: Record<string, ActionPoolDefinition> = {
  "weapon.sword.basic": {
    id: "weapon.sword.basic",
    actions: ["sword.stab_a", "sword.slash_a", "sword.uppercut_a"],
    profiles: {
      male: replaceWith([
        candidate("sword.male.slash_drive", 3, ["slash", "heavy"]),
        candidate("sword.male.uppercut_drive", 2, ["uppercut", "heavy"]),
        candidate("sword.male.stab_drive", 1, ["stab"])
      ]),
      female: replaceWith([
        candidate("sword.female.stab_lunge", 3, ["stab", "quick"]),
        candidate("sword.female.slash_lunge", 2, ["slash", "quick"]),
        candidate("sword.female.uppercut_lunge", 2, ["uppercut", "quick"])
      ])
    }
  },
  "weapon.fist.basic": {
    id: "weapon.fist.basic",
    actions: ["fist.punch", "fist.heavy", "fist.kick"],
    profiles: {
      male: replaceWith([
        candidate("fist.male.heavy_drive", 3, ["heavy", "strike"]),
        candidate("fist.male.punch_drive", 2, ["punch", "strike"]),
        candidate("fist.male.kick_drive", 1, ["kick"])
      ]),
      female: replaceWith([
        candidate("fist.female.kick_lunge", 3, ["kick", "quick"]),
        candidate("fist.female.punch_lunge", 2, ["punch", "quick"]),
        candidate("fist.female.heavy_lunge", 1, ["heavy"])
      ])
    }
  },
  "skill.sword_focus": {
    id: "skill.sword_focus",
    actions: ["skill.sword_focus.guard"],
    profiles: {
      male: replaceWith(["skill.sword_focus.male_guard"]),
      female: replaceWith(["skill.sword_focus.female_guard"])
    }
  },
  "skill.fist_focus": {
    id: "skill.fist_focus",
    actions: ["skill.fist_focus.guard", "skill.fist_focus.palm"],
    profiles: {
      male: replaceWith([candidate("skill.fist_focus.male_guard", 2, ["buff", "stance"]), candidate("skill.fist_focus.male_palm", 1, ["heal"])]),
      female: replaceWith([candidate("skill.fist_focus.female_palm", 2, ["heal"]), candidate("skill.fist_focus.female_guard", 1, ["buff", "stance"])])
    }
  },
  "skill.self_focus": {
    id: "skill.self_focus",
    actions: ["skill.self_focus.guard", "skill.self_focus.palm"],
    styles: {
      sword: replaceWith(["skill.sword_focus.guard"]),
      fist: replaceWith(["skill.fist_focus.guard", "skill.fist_focus.palm"])
    },
    styleProfiles: {
      sword: {
        male: replaceWith(["skill.sword_focus.male_guard"]),
        female: replaceWith(["skill.sword_focus.female_guard"])
      },
      fist: {
        male: replaceWith([candidate("skill.fist_focus.male_guard", 2, ["buff", "stance"]), candidate("skill.fist_focus.male_palm", 1, ["heal"])]),
        female: replaceWith([candidate("skill.fist_focus.female_palm", 2, ["heal"]), candidate("skill.fist_focus.female_guard", 1, ["buff", "stance"])])
      }
    }
  },
  "effect.tick": {
    id: "effect.tick",
    actions: ["effect.dot", "effect.hot"]
  }
};

export function visualProfileFromGender(gender: string | null | undefined): VisualProfile {
  return gender === "female" ? "female" : "male";
}

export function combatStyleFromSnapshot(style: string | null | undefined): CombatStyle {
  return style === "sword" ? "sword" : "fist";
}

export function idleVisualForStyle(style: CombatStyle, profile: VisualProfile): ActorVisual {
  return {
    kind: "sprite",
    sprite: idleSpriteForStyle(style, profile)
  };
}

export function reactionVisualFor(reaction: Exclude<TargetReaction, "none">, profile: VisualProfile, style: CombatStyle): ActorVisual {
  return {
    kind: "sprite",
    sprite: reactionSpriteFor(reaction, profile, style)
  };
}

export function visualForClip(clipId: string, profile: VisualProfile = "male", fallbackStyle: CombatStyle = "fist"): ActorVisual {
  return {
    kind: "sprite",
    sprite: spriteForClip(clipId, profile, fallbackStyle)
  };
}

export function idleSpriteForStyle(style: CombatStyle, profile: VisualProfile): string {
  const clip = spriteClips[`actor.${style}.idle`];
  return clip?.sprites[profile] || clip?.sprites.male || fistMaleIdle;
}

export function reactionSpriteFor(reaction: Exclude<TargetReaction, "none">, profile: VisualProfile, style: CombatStyle = "fist"): string {
  const clipId = reactionClips[reaction];
  return clipId ? spriteForClip(clipId, profile, style) : idleSpriteForStyle(style, profile);
}

export function spriteForClip(clipId: string, profile: VisualProfile = "male", fallbackStyle: CombatStyle = "fist"): string {
  return spriteClips[clipId]?.sprites[profile] || spriteClips[clipId]?.sprites.male || idleSpriteForStyle(fallbackStyle, profile);
}
