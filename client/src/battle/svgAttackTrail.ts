import type { ActorVisual, ResolvedBattleTimeline } from './animationTypes';
import { sampleBattleScene, sideHome } from './battleDirector';
import { sampleSvgPose, SVG_SWORD_LENGTH } from './svgBattlePose';

/** Sample the actual weapon/limb path; effects share the clock, including hit stop. */
export function sampleAttackTrail(timeline: ResolvedBattleTimeline | null, elapsed: number, player: ActorVisual, enemy: ActorVisual, reduced = false) {
  if (!timeline || reduced || !['approach', 'lunge', 'drive'].includes(timeline.actor.motion)) return { points: '', echoes: [] };
  const c = timeline.choreography;
  const impact = timeline.impactAtMs;
  const t = elapsed >= impact && elapsed < impact + c.hitStopMs ? impact : elapsed;
  const side = timeline.actor.side;
  const mirror = side === 'player' ? 1 : -1;
  const sample = (ms: number) => {
    const figure = sampleBattleScene(timeline, ms, player, enemy)[side];
    return { figure, pose: sampleSvgPose(timeline, side, ms, figure.visual.style, figure.frameId), x: sideHome(side) + figure.x };
  };
  const start = Math.max(c.launchAtMs, t - 125);
  const points = Array.from({ length: 12 }, (_, i) => {
    const { pose, figure, x } = sample(start + Math.max(0, t - start) * i / 11);
    const kick = timeline.actor.visual.actionId.includes('kick');
    const limb = kick ? pose.foot : pose.hand;
    const length = figure.visual.style === 'sword' ? SVG_SWORD_LENGTH : 0;
    const angle = pose.blade * Math.PI / 180;
    return `${x + mirror * (limb[0] + Math.cos(angle) * length)},${figure.y + limb[1] + Math.sin(angle) * length}`;
  }).join(' ');
  const echoes = [70, 35].map(delay => sample(Math.max(0, t - delay)));
  return { points, echoes };
}
