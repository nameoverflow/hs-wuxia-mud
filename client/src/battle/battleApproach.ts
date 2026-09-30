/** Brief planted compression, then a single accelerating launch and sharp arrival. */
export function dashProgress(phase: number) {
  const p = Math.max(0, Math.min(1, (phase - 0.5) / 0.5));
  return p * p;
}

/**
 * 写意身法：不是滑过去，而是一帧换位。
 * 前半段原地扎住架势，过了中点直接落到对手身前，位移交给残墨和斩击特效解释。
 */
export function entryAt(phase: number) {
  return phase < 0.5 ? 0 : 1;
}
