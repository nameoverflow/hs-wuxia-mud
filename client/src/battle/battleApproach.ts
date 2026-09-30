/**
 * 冲刺：一张冲刺姿势保持不动，整个人连续推过去——起步最快、临近对手时收住。
 * 参考录屏里冲刺是匀速略带减速的明显位移，而不是瞬移。
 */
export function dashIn(phase: number) {
  const p = Math.max(0, Math.min(1, phase));
  return 1 - (1 - p) * (1 - p);
}

/** 后撤：起步和落位都收一点，整段连续滑回原位。 */
export function dashOut(phase: number) {
  const p = Math.max(0, Math.min(1, phase));
  return p * p * (3 - 2 * p);
}
