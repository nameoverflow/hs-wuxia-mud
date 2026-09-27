/** Per-action footwork. These are presentation lead-ins, not server combat time. */
export interface BattleApproach {
  kind: 'step-in' | 'palm-drive' | 'knee-hop' | 'sword-glide' | 'cross-step' | 'raised-step' | 'low-skate';
  durationMs: number;
  lift: number;
}
export const battleApproaches: Record<string, BattleApproach> = {
  'rig.fist.punch_a': { kind: 'step-in', durationMs: 150, lift: 3 },
  'rig.fist.heavy_a': { kind: 'palm-drive', durationMs: 165, lift: 1 },
  'rig.fist.kick_a': { kind: 'knee-hop', durationMs: 165, lift: 5 },
  'rig.sword.thrust_a': { kind: 'sword-glide', durationMs: 140, lift: 2 },
  'rig.sword.cut_a': { kind: 'cross-step', durationMs: 155, lift: 2 },
  'rig.sword.chop_a': { kind: 'raised-step', durationMs: 160, lift: 3 },
  'rig.sword.rising_cut_a': { kind: 'low-skate', durationMs: 155, lift: 1 }
};
export function approachForAction(actionId: string) { return battleApproaches[actionId]; }

/** Brief planted compression, then a single accelerating launch and sharp arrival. */
export function dashProgress(phase: number) {
  const p = Math.max(0, Math.min(1, (phase - 0.5) / 0.5));
  return p * p;
}
