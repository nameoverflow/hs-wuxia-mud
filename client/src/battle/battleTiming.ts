import type { OffsetKeyDefinition, PoseKeyDefinition, ResolvedBattleTimeline, ResolvedHit } from "./animationTypes";

/**
 * 命中定格的时间扭曲：处在某一段定格里时，所有画面层都读同一个接触瞬间。
 * 定格吃掉的是动作时间，不是把后面整体顺延。
 */
export function visualTimeAt(timeline: ResolvedBattleTimeline, t: number) {
  for (const hit of timeline.hits) if (t >= hit.atMs && t < hit.atMs + hit.hitStopMs) return hit.atMs;
  return t;
}

/** 最近一次已发生的命中；尚未命中时为 -1。 */
export function hitIndexAt(timeline: ResolvedBattleTimeline, t: number) {
  let index = -1;
  timeline.hits.forEach((hit, i) => { if (t >= hit.atMs) index = i; });
  return index;
}

export function lastHit(timeline: ResolvedBattleTimeline) {
  return timeline.hits[timeline.hits.length - 1];
}

/** 当前生效的姿势键：最后一个 atMs ≤ t 的键。 */
export function poseKeyAt(keys: PoseKeyDefinition[], t: number) {
  let index = -1;
  keys.forEach((key, i) => { if (t >= key.atMs) index = i; });
  return keys[index];
}

/** 轨迹追踪点：当前键若是接触键就用它的点，否则用下一个接触键的点。 */
export function reachPointAt(timeline: ResolvedBattleTimeline, t: number) {
  const keys = timeline.actor.poseKeys;
  const current = poseKeyAt(keys, t);
  if (current?.pin) return current.pin;
  return keys.find((key) => key.atMs > t && key.pin)?.pin ?? [...keys].reverse().find((key) => key.pin)?.pin ?? "hand";
}

/** 闪避提前、招架稍早、受击严格在接触帧；提前量都来自该段的 staging。 */
export function reactionStartAt(timeline: ResolvedBattleTimeline, hit: ResolvedHit) {
  // 对手冲到面前、开始蓄势时就可以起反应，不必等出手那一帧。
  const launch = timeline.actor.actionDelayMs;
  if (hit.reaction === "dodge") return Math.max(launch, hit.atMs - (hit.staging.reactions.dodge.leadMs ?? 0));
  if (hit.reaction === "parry") return Math.max(launch, hit.atMs - (hit.staging.reactions.parry.leadMs ?? 0));
  return hit.atMs;
}

/**
 * 当前生效的受击段：最后一个反应已开始的段。多段里可以先中后闪。
 * 连续同类反应视为一整段：startAt 取这一串的第一段，闪避不会每段都退回原位重来。
 */
export function activeReactionAt(timeline: ResolvedBattleTimeline, t: number) {
  let active: { hit: ResolvedHit; index: number; startAt: number } | undefined;
  timeline.hits.forEach((hit, index) => {
    const startAt = reactionStartAt(timeline, hit);
    if (t < startAt) return;
    const continues = active && active.index === index - 1 && active.hit.reaction === hit.reaction;
    active = { hit, index, startAt: continues ? active!.startAt : startAt };
  });
  return active;
}

/** 第 n 个接触键对应第 n 段命中。 */
export function hitForPinnedKey(timeline: ResolvedBattleTimeline, key: PoseKeyDefinition) {
  const index = timeline.actor.poseKeys.filter((k) => k.pin).indexOf(key);
  return timeline.hits[Math.max(0, Math.min(index, timeline.hits.length - 1))];
}

const easeOut = (v: number) => 1 - (1 - v) ** 3;

/** 人物根节点的附加位移（x 朝向对手为正，y 向下为正）与倾角，读的是定格后的时间。 */
export function actorOffsetAt(timeline: ResolvedBattleTimeline, t: number) {
  const keys = timeline.actor.offsetKeys;
  const value = (key: OffsetKeyDefinition | undefined) => ({ x: key?.x ?? 0, y: key?.y ?? 0, angle: key?.angle ?? 0 });
  if (!keys.length || t < keys[0].atMs) return value(undefined);
  let index = 0;
  keys.forEach((key, i) => { if (t >= key.atMs) index = i; });
  const from = value(keys[index]);
  const nextKey = keys[index + 1];
  if (!nextKey || (nextKey.ease ?? "cut") === "cut") return from;
  const to = value(nextKey);
  const raw = Math.max(0, Math.min(1, (t - keys[index].atMs) / Math.max(1, nextKey.atMs - keys[index].atMs)));
  const p = nextKey.ease === "out" ? easeOut(raw) : raw;
  return { x: from.x + (to.x - from.x) * p, y: from.y + (to.y - from.y) * p, angle: from.angle + (to.angle - from.angle) * p };
}
