import type { PoseKeyDefinition, ResolvedBattleTimeline, ResolvedHit } from "./animationTypes";

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

export function currentHit(timeline: ResolvedBattleTimeline, t: number): ResolvedHit | undefined {
  return timeline.hits[hitIndexAt(timeline, t)];
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
