import { approachForAction, dashProgress } from "./battleApproach";
import { clipFrameAt } from "./animationClip";
import type { ActorVisual, BattleSide, ResolvedBattleTimeline } from "./animationTypes";

export interface FigureSample {
  visual: ActorVisual;
  frameId: string;
  x: number;
  y: number;
  angle: number;
  alpha: number;
  flash: number;
}

export interface BattleSceneSample {
  player: FigureSample;
  enemy: FigureSample;
  phase: "idle" | "approach" | "prepare" | "strike" | "impact" | "recover" | "settlement";
  cameraScale: number;
  cameraX: number;
  shade: number;
  trail: number;
  burst: number;
  guard: number;
  aura: number;
  ghost: number;
  textAlpha: number;
  textLift: number;
  contact: { x: number; y: number };
  resultAlpha: number;
}

const clamp = (v: number) => Math.max(0, Math.min(1, v));
const progress = (t: number, a: number, b: number) => clamp((t - a) / Math.max(1, b - a));
const smooth = (v: number) => v * v * (3 - 2 * v);
const out = (v: number) => 1 - (1 - v) ** 3;

/** Pure, deterministic staging in source-art pixels. No renderer-owned tweens. */
export function sampleBattleScene(
  timeline: ResolvedBattleTimeline | null,
  elapsedMs: number,
  idlePlayer: ActorVisual,
  idleEnemy: ActorVisual,
  reducedMotion = false
): BattleSceneSample {
  const idle = (visual: ActorVisual): FigureSample => ({ visual, frameId: visual.frames[0].frameId, x: 0, y: 0, angle: 0, alpha: 1, flash: 0 });
  const result: BattleSceneSample = {
    player: idle(idlePlayer), enemy: idle(idleEnemy), phase: "idle", cameraScale: 1, cameraX: 0,
    shade: 0, trail: 0, burst: 0, guard: 0, aura: 0, ghost: 0, textAlpha: 0, textLift: 0,
    contact: { x: 0, y: -70 }, resultAlpha: 0
  };
  if (!timeline) return result;
  const t = Math.max(0, Math.min(elapsedMs, timeline.durationMs));
  if (timeline.kind === "settlement") {
    result.phase = "settlement";
    result.resultAlpha = out(progress(t, 0, 180)) * (1 - progress(t, timeline.durationMs - 160, timeline.durationMs));
    result.shade = result.resultAlpha * 0.38;
    if (!reducedMotion) {
      const loser = timeline.target.side === "player" ? result.player : result.enemy;
      loser.alpha = 1 - out(progress(t, 80, 480)) * 0.7;
      loser.y = out(progress(t, 80, 480)) * 5;
    }
    return result;
  }

  const c = timeline.choreography;
  const arrival = timeline.actor.actionDelayMs;
  const impact = timeline.impactAtMs;
  const holdEnd = impact + c.hitStopMs;
  // During hit stop every moving layer reads the same held instant.
  const visualTime = t >= impact && t < holdEnd ? impact : t;
  const actor = timeline.actor.side === "player" ? result.player : result.enemy;
  const target = timeline.target.side === "player" ? result.player : result.enemy;
  const direction = timeline.actor.side === "player" ? 1 : -1;
  const targetDirection = timeline.target.side === "player" ? -1 : 1;
  const attack = ["approach", "lunge", "drive"].includes(timeline.actor.motion);
  const quiet = timeline.kind === "effect_tick";
  const heavy = c.weight === "heavy";
  const recovery = smooth(progress(visualTime, c.recoverAtMs, c.restAtMs));
  const envelope = quiet ? 0 : out(progress(visualTime, 0, Math.max(1, c.launchAtMs))) * (1 - recovery);
  result.phase = t < arrival ? "approach" : t < c.launchAtMs ? "prepare" : t < impact ? "strike" : t < c.recoverAtMs ? "impact" : t < c.restAtMs ? "recover" : "idle";

  if (!quiet) {
    actor.visual = timeline.actor.visual;
    actor.frameId = t < arrival ? `approach-${approachForAction(actor.visual.actionId)?.kind}`
      : clipFrameAt(actor.visual.frames, Math.max(0, visualTime - arrival), timeline.durationMs - arrival).frame.frameId;
  }

  if (attack) {
    const travel = Math.max(8, sideHome("enemy") - sideHome("player") - c.reach);
    const entry = dashProgress(progress(visualTime, 0, arrival));
    actor.x = direction * travel * entry * (1 - recovery);
    const approach = approachForAction(timeline.actor.visual.actionId);
    actor.y = visualTime < arrival ? -(approach?.lift ?? 0) * Math.sin(Math.PI * progress(visualTime, 0, arrival)) : 0;
    result.contact.x = sideHome(timeline.target.side);
    result.contact.y = c.contactY - 176;
  } else {
    result.contact.x = sideHome(timeline.target.side);
    result.contact.y = -70;
  }

  const reaction = timeline.target.reaction;
  // Dodge anticipates the contact; parry prepares shortly before it; hurt never does.
  const reactionAt = reaction === "dodge" ? Math.max(c.launchAtMs, impact - 95) : reaction === "parry" ? Math.max(c.launchAtMs, impact - 50) : impact;
  if (!quiet && actor !== target && t >= reactionAt && t < c.restAtMs && reaction !== "none" && reaction !== "effect") {
    target.visual = timeline.target.visual;
    target.frameId = target.visual.frames[0].frameId;
    const reactionTime = reaction === "hit" ? impact + Math.max(0, t - holdEnd) : visualTime;
    const onset = out(progress(reactionTime, reactionAt, reactionAt + (reaction === "dodge" ? 85 : 65)));
    const amount = onset * (1 - recovery);
    target.x = targetDirection * (reaction === "dodge" ? 52 : reaction === "parry" ? 4 : heavy ? 22 : 13) * amount;
    target.angle = targetDirection * (reaction === "hit" ? 7 : 0) * amount;
    result.ghost = reaction === "dodge" ? 0.32 * amount : 0;
  }

  if (t >= impact) {
    const feedbackTime = visualTime === impact ? 0 : visualTime - holdEnd;
    const feedback = 1 - out(clamp(feedbackTime / 180));
    const textEnd = Math.min(timeline.durationMs, Math.max(impact + 300, c.restAtMs));
    result.textAlpha = 1 - smooth(progress(t, Math.max(impact + 150, textEnd - 150), textEnd));
    result.textLift = 15 * out(progress(visualTime, holdEnd, textEnd));
    if (timeline.result === "hit") {
      result.burst = feedback;
      target.flash = feedback * 0.48;
    }
    if (timeline.result === "parry") result.guard = feedback;
    if (timeline.heal || timeline.actor.motion === "focus") result.aura = (1 - recovery) * 0.65;
    if (!quiet && !reducedMotion) {
      result.cameraX = (heavy ? 2.8 : 1) * Math.cos(Math.max(0, feedbackTime) * 0.08) * feedback;
    }
  }

  if (attack && t >= impact - 85 && t < holdEnd + 170) {
    result.trail = t < impact ? progress(t, impact - 85, impact) : 1 - out(progress(visualTime, holdEnd, holdEnd + 170));
  }
  result.cameraScale = 1 + envelope * (heavy ? 0.035 : 0.015);
  result.shade = envelope * (heavy ? 0.16 : 0.06);
  if (reducedMotion) {
    for (const figure of [result.player, result.enemy]) { figure.x = 0; figure.y = 0; figure.angle = 0; }
    result.cameraScale = 1;
    result.cameraX = 0;
    result.trail = 0;
    result.ghost = 0;
    result.textLift = 0;
    result.burst *= 0.4;
  }
  return result;
}

export function sideHome(side: BattleSide) { return side === "player" ? -140 : 140; }
