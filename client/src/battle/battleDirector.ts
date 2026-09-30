import { dashIn, dashOut } from "./battleApproach";
import { clipFrameAt } from "./animationClip";
import { activeReactionAt, actorOffsetAt, hitIndexAt, lastHit, visualTimeAt } from "./battleTiming";
import { TRAVEL_MOTIONS, type ActorVisual, type BattleSide, type ResolvedBattleTimeline, type StagingProfile } from "./animationTypes";

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
  const staging = timeline.staging;
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
  const attack = TRAVEL_MOTIONS.includes(timeline.actor.motion);
  const strikes = attack || timeline.actor.motion === "ranged";
  const quiet = timeline.kind === "effect_tick";
  const retreat = progress(visualTime, c.recoverAtMs, c.restAtMs);
  const recovery = smooth(retreat);
  const envelope = quiet ? 0 : out(progress(visualTime, 0, Math.max(1, c.launchAtMs))) * (1 - recovery);
  result.force = quiet || !strikes ? 0 : staging.force;
  result.phase = t < arrival ? "approach" : t < c.launchAtMs ? "prepare" : t < impact ? "strike" : t < c.recoverAtMs ? "impact" : t < c.restAtMs ? "recover" : "idle";

  if (!quiet) {
    actor.visual = timeline.actor.visual;
    actor.frameId = t < arrival && timeline.actor.approach ? timeline.actor.approach.pose
      : clipFrameAt(actor.visual.frames, Math.max(0, visualTime - arrival), timeline.durationMs - arrival).frame.frameId;
  }

  result.contact.x = sideHome(timeline.target.side);
  result.contact.y = strikes ? c.contactY - 176 : -70;
  if (attack) {
    const travel = Math.max(8, sideHome("enemy") - sideHome("player") - c.reach);
    // 冲刺与后撤都是连续位移：姿势定住，整个人滑过去、再滑回来，离地一点，影子留在地上。
    const dash = progress(visualTime, 0, arrival);
    actor.x = direction * travel * dashIn(dash) * (1 - dashOut(retreat));
    const lift = timeline.actor.approach?.lift ?? 0;
    const arc = (p: number) => (p <= 0 || p >= 1 ? 0 : Math.sin(Math.PI * p));
    actor.y = (-lift * arc(dash) - lift * 0.6 * arc(retreat)) || 0;
  }
  if (!quiet && timeline.actor.offsetKeys.length) {
    // 身法轨道：跃起、后翻、滑步等附加位移，叠加在站位之上。
    const offset = actorOffsetAt(timeline, visualTime);
    actor.x += direction * offset.x;
    actor.y += offset.y;
    actor.angle += direction * offset.angle;
  }

  const active = quiet || actor === target ? undefined : activeReactionAt(timeline, t);
  if (active && t < c.restAtMs && active.hit.reaction !== "none" && active.hit.reaction !== "effect") {
    const { hit: reactionHit, startAt } = active;
    const reaction = reactionHit.reaction as keyof StagingProfile["reactions"];
    const look = reactionHit.staging.reactions[reaction];
    target.visual = reactionHit.targetVisual;
    target.frameId = target.visual.frames[0].frameId;
    const reactionTime = reaction === "hit" ? reactionHit.atMs + Math.max(0, t - reactionHit.atMs - reactionHit.hitStopMs) : visualTime;
    // 受击是硬切：到点直接到位，不做渐进。
    const onset = reaction === "hit" ? 1 : out(progress(reactionTime, startAt, startAt + (look.onsetMs ?? 0)));
    const amount = onset * (1 - recovery);
    // 受击先被打退一大截，定格结束后再踉跄滑出剩下一段；闪避整个人跳开，带一段离地弧线。
    const holdEnd = reactionHit.atMs + reactionHit.hitStopMs;
    const stagger = reaction === "hit" ? (look.snap ?? 1) + (1 - (look.snap ?? 1)) * out(progress(t, holdEnd, holdEnd + 200)) : 1;
    target.x = targetDirection * look.push * amount * stagger;
    if (reaction === "hit" && look.lift) target.y = -look.lift * amount;
    if (reaction === "dodge" && look.hop) {
      const jump = progress(visualTime, startAt, startAt + 300);
      if (jump > 0 && jump < 1) target.y = -look.hop * Math.sin(Math.PI * jump) * (1 - recovery);
    }
    target.angle = targetDirection * (reaction === "hit" ? look.tilt ?? 0 : 0) * amount;
    result.ghost = reaction === "dodge" ? (look.ghost ?? 0) * amount : 0;
  }

  if (t >= impact) {
    const feedbackTime = visualTime === hit.atMs ? 0 : visualTime - hitHoldEnd;
    // 衰减必须在时间线结束前走完，否则尾部会残留火星和招架环。
    const fall = Math.min(hit.staging.fallMs, Math.max(1, timeline.durationMs - hitHoldEnd));
    const feedback = 1 - decayAfterHold(visualTime, hitHoldEnd, fall);
    const textEnd = Math.min(timeline.durationMs, Math.max(final.atMs + 340, c.restAtMs));
    result.textAlpha = 1 - smooth(progress(t, Math.max(final.atMs + 190, textEnd - 190), textEnd));
    result.textLift = 26 * out(progress(visualTime, holdEnd, textEnd));
    result.textScale = 0.84 + 0.16 * out(progress(visualTime, hit.atMs, hit.atMs + 120));
    if (hit.result === "hit") {
      result.burst = feedback;
      target.flash = feedback * hit.staging.flash;
    }
    if (hit.result === "parry") result.guard = feedback;
    if (timeline.heal || timeline.actor.motion === "focus") result.aura = (1 - recovery) * 0.65;
    if (!quiet && !reducedMotion) {
      // 沿攻击方向砸一下，回弹两次收敛，而不是原地的余弦晃动。
      const { kick, rebound, lift } = hit.staging.camera;
      const decay = feedback ** 2;
      result.cameraX = direction * kick * decay * Math.cos(Math.max(0, feedbackTime) * 0.045);
      result.cameraX += direction * kick * decay * Math.cos(Math.max(0, feedbackTime) * 0.11) * rebound;
      result.shakeY = -kick * lift * decay * Math.cos(Math.max(0, feedbackTime) * 0.05);
    }
  }

  // 每一段各有一次起笔和收笔；取最晚进入窗口的那一段。
  const { leadMs, fadeMs } = staging.trail;
  const trailHit = attack ? [...timeline.hits].reverse().find((h) => t >= h.atMs - leadMs && t < h.atMs + h.hitStopMs + fadeMs) : undefined;
  if (trailHit) {
    const end = trailHit.atMs + trailHit.hitStopMs;
    result.trail = t < trailHit.atMs ? progress(t, trailHit.atMs - leadMs, trailHit.atMs) : 1 - out(progress(visualTime, end, end + fadeMs));
  }
  // 定格期间压暗并反白，只持续一两帧，是这套写意风格里最直接的打击反馈。
  if (!quiet && !reducedMotion && timeline.hits.some((h) => h.hitStopMs > 0 && t >= h.atMs && t < h.atMs + h.hitStopMs)) result.invert = 1;
  result.angle = quiet ? 0 : staging.tilt * Math.sin(Math.PI * progress(t, 0, Math.max(1, c.restAtMs))) * (t < c.restAtMs ? 1 : 0);
  // 镜头硬切：蓄势时不动，命中瞬间直接推近，随后保持，不做缓动。
  const cutIn = quiet ? 0 : progress(visualTime, c.launchAtMs, impact);
  result.cameraScale = 1 + (cutIn >= 1 ? 1 : envelope * 0.35) * staging.camera.zoom;
  result.shade = envelope * staging.shade + result.invert * 0.25;
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

/** 双方站位相距约四个身位，冲刺才看得出距离。 */
export const SIDE_HOME = 230;
export function sideHome(side: BattleSide) { return side === "player" ? -SIDE_HOME : SIDE_HOME; }
