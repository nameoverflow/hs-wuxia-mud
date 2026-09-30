import { entryAt } from "./battleApproach";
import { clipFrameAt } from "./animationClip";
import { hitIndexAt, lastHit, visualTimeAt } from "./battleTiming";
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
  shakeY: number;
  shade: number;
  trail: number;
  burst: number;
  guard: number;
  aura: number;
  ghost: number;
  /** 命中定格：舞台压暗、人物反白，只持续一两帧。 */
  invert: number;
  angle: number;
  textAlpha: number;
  textLift: number;
  textScale: number;
  contact: { x: number; y: number };
  /** 0=未命中，1=重招命中，供舞台决定镜头切近与墨爆大小。 */
  force: number;
  /** 最近一次已发生的命中段；-1 表示尚未接触。 */
  hitIndex: number;
  resultAlpha: number;
}

const clamp = (v: number) => Math.max(0, Math.min(1, v));
const progress = (t: number, a: number, b: number) => clamp((t - a) / Math.max(1, b - a));
const smooth = (v: number) => v * v * (3 - 2 * v);
const out = (v: number) => 1 - (1 - v) ** 3;

/**
 * 命中定格：定格多久就是多久，不做缓动。
 * 定格结束后才进入余劲衰减，这里返回的是 0→1 的衰减量。
 */
function decayAfterHold(visualTime: number, holdEnd: number, fallMs: number) {
  return out(clamp((visualTime - holdEnd) / Math.max(1, fallMs)));
}

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
    player: idle(idlePlayer), enemy: idle(idleEnemy), phase: "idle", cameraScale: 1, cameraX: 0, shakeY: 0,
    shade: 0, trail: 0, burst: 0, guard: 0, aura: 0, ghost: 0, invert: 0, angle: 0, textAlpha: 0, textLift: 0,
    textScale: 1, contact: { x: 0, y: -70 }, force: 0, hitIndex: -1, resultAlpha: 0
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
  const visualTime = visualTimeAt(timeline, t);
  result.hitIndex = hitIndexAt(timeline, t);
  // 多段命中时，火花、闪白、镜头回弹都跟着最近一段走；飘字的淡出以最后一段为准。
  const hit = timeline.hits[Math.max(0, result.hitIndex)];
  const hitHoldEnd = hit.atMs + hit.hitStopMs;
  const final = lastHit(timeline);
  const actor = timeline.actor.side === "player" ? result.player : result.enemy;
  const target = timeline.target.side === "player" ? result.player : result.enemy;
  const direction = timeline.actor.side === "player" ? 1 : -1;
  const targetDirection = timeline.target.side === "player" ? -1 : 1;
  const attack = ["approach", "lunge", "drive"].includes(timeline.actor.motion);
  const quiet = timeline.kind === "effect_tick";
  const heavy = c.weight === "heavy";
  const recovery = smooth(progress(visualTime, c.recoverAtMs, c.restAtMs));
  const envelope = quiet ? 0 : out(progress(visualTime, 0, Math.max(1, c.launchAtMs))) * (1 - recovery);
  result.force = quiet || !attack ? 0 : heavy ? 1 : 0.6;
  result.phase = t < arrival ? "approach" : t < c.launchAtMs ? "prepare" : t < impact ? "strike" : t < c.recoverAtMs ? "impact" : t < c.restAtMs ? "recover" : "idle";

  if (!quiet) {
    actor.visual = timeline.actor.visual;
    actor.frameId = t < arrival && timeline.actor.approach ? timeline.actor.approach.pose
      : clipFrameAt(actor.visual.frames, Math.max(0, visualTime - arrival), timeline.durationMs - arrival).frame.frameId;
  }

  if (attack) {
    const travel = Math.max(8, sideHome("enemy") - sideHome("player") - c.reach);
    // 一帧换位，不是滑行；回位用一次硬切的撤步，收在两拍内。
    actor.x = direction * travel * entryAt(progress(visualTime, 0, arrival)) * (1 - hardStep(recovery));
    actor.y = visualTime < arrival ? -(timeline.actor.approach?.lift ?? 0) * Math.sin(Math.PI * progress(visualTime, 0, arrival)) : 0;
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
    // 受击是硬切：到点直接到位，不做渐进。
    const onset = reaction === "hit" ? 1 : out(progress(reactionTime, reactionAt, reactionAt + (reaction === "dodge" ? 85 : 65)));
    const amount = onset * (1 - hardStep(recovery));
    target.x = targetDirection * (reaction === "dodge" ? 52 : reaction === "parry" ? 4 : heavy ? 48 : 26) * amount;
    target.angle = targetDirection * (reaction === "hit" ? (heavy ? 14 : 9) : 0) * amount;
    result.ghost = reaction === "dodge" ? 0.32 * amount : 0;
  }

  if (t >= impact) {
    const feedbackTime = visualTime === hit.atMs ? 0 : visualTime - hitHoldEnd;
    // 衰减必须在时间线结束前走完，否则尾部会残留火星和招架环。
    const fall = Math.min(heavy ? 260 : 180, Math.max(1, timeline.durationMs - hitHoldEnd));
    const feedback = 1 - decayAfterHold(visualTime, hitHoldEnd, fall);
    const textEnd = Math.min(timeline.durationMs, Math.max(final.atMs + 340, c.restAtMs));
    result.textAlpha = 1 - smooth(progress(t, Math.max(final.atMs + 190, textEnd - 190), textEnd));
    result.textLift = 26 * out(progress(visualTime, holdEnd, textEnd));
    result.textScale = 0.84 + 0.16 * out(progress(visualTime, hit.atMs, hit.atMs + 120));
    if (timeline.result === "hit") {
      result.burst = feedback;
      target.flash = feedback * (heavy ? 1 : 0.75);
    }
    if (timeline.result === "parry") result.guard = feedback;
    if (timeline.heal || timeline.actor.motion === "focus") result.aura = (1 - recovery) * 0.65;
    if (!quiet && !reducedMotion) {
      // 沿攻击方向砸一下，回弹两次收敛，而不是原地的余弦晃动。
      const kick = heavy ? 11 : 5.5;
      const decay = feedback ** 2;
      result.cameraX = direction * kick * decay * Math.cos(Math.max(0, feedbackTime) * 0.045);
      result.cameraX += direction * kick * decay * Math.cos(Math.max(0, feedbackTime) * 0.11) * 0.35;
      result.shakeY = -kick * 0.35 * decay * Math.cos(Math.max(0, feedbackTime) * 0.05);
    }
  }

  // 每一段各有一次起笔和收笔；取最晚进入窗口的那一段。
  const trailHit = attack ? [...timeline.hits].reverse().find((h) => t >= h.atMs - 85 && t < h.atMs + h.hitStopMs + 170) : undefined;
  if (trailHit) {
    const end = trailHit.atMs + trailHit.hitStopMs;
    result.trail = t < trailHit.atMs ? progress(t, trailHit.atMs - 85, trailHit.atMs) : 1 - out(progress(visualTime, end, end + 170));
  }
  // 定格期间压暗并反白，只持续一两帧，是这套写意风格里最直接的打击反馈。
  if (!quiet && !reducedMotion && timeline.hits.some((h) => h.hitStopMs > 0 && t >= h.atMs && t < h.atMs + h.hitStopMs)) result.invert = 1;
  result.angle = quiet ? 0 : (heavy ? 2.4 : 1) * Math.sin(Math.PI * progress(t, 0, Math.max(1, c.restAtMs))) * (t < c.restAtMs ? 1 : 0);
  // 镜头硬切：蓄势时不动，命中瞬间直接推近，随后保持，不做缓动。
  const cutIn = quiet ? 0 : progress(visualTime, c.launchAtMs, impact);
  result.cameraScale = 1 + (cutIn >= 1 ? 1 : envelope * 0.35) * (heavy ? 0.1 : 0.045);
  result.shade = envelope * (heavy ? 0.22 : 0.09) + result.invert * 0.25;
  if (reducedMotion) {
    for (const figure of [result.player, result.enemy]) { figure.x = 0; figure.y = 0; figure.angle = 0; }
    result.cameraScale = 1;
    result.cameraX = 0;
    result.shakeY = 0;
    result.angle = 0;
    result.trail = 0;
    result.ghost = 0;
    result.textLift = 0;
    result.invert = 0;
    result.textScale = 1;
    result.burst *= 0.4;
  }
  return result;
}

/** 回位不走平滑插值：过了阈值直接切到落位，两拍内收住。 */
function hardStep(recovery: number) {
  return recovery < 0.55 ? 0 : 1;
}

export function sideHome(side: BattleSide) { return side === "player" ? -140 : 140; }
