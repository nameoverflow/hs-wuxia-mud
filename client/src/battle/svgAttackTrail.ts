import type { ActorVisual, ResolvedBattleTimeline } from './animationTypes';
import { sampleBattleScene, sideHome } from './battleDirector';
import { sampleSvgPose, SVG_SWORD_LENGTH } from './svgBattlePose';
import { reachPointAt, visualTimeAt } from './battleTiming';

/** Sample the actual weapon/limb path; effects share the clock, including hit stop. */
export function sampleAttackTrail(timeline: ResolvedBattleTimeline | null, elapsed: number, player: ActorVisual, enemy: ActorVisual, reduced = false) {
  if (!timeline || reduced || !['approach', 'lunge', 'drive'].includes(timeline.actor.motion)) return { points: '', echoes: [] };
  const c = timeline.choreography;
  const t = visualTimeAt(timeline, elapsed);
  const side = timeline.actor.side;
  const mirror = side === 'player' ? 1 : -1;
  const sample = (ms: number) => {
    const figure = sampleBattleScene(timeline, ms, player, enemy)[side];
    return { figure, pose: sampleSvgPose(timeline, side, ms, figure.visual.style, figure.frameId), x: sideHome(side) + figure.x };
  };
  const start = Math.max(c.launchAtMs, t - 125);
  const points = Array.from({ length: 12 }, (_, i) => {
    const ms = start + Math.max(0, t - start) * i / 11;
    const { pose, figure, x } = sample(ms);
    const reachWith = reachPointAt(timeline, visualTimeAt(timeline, ms));
    const limb = reachWith === 'foot' ? pose.foot : pose.hand;
    const length = reachWith === 'blade' ? SVG_SWORD_LENGTH : 0;
    const angle = pose.blade * Math.PI / 180;
    return `${x + mirror * (limb[0] + Math.cos(angle) * length)},${figure.y + limb[1] + Math.sin(angle) * length}`;
  }).join(' ');
  const echoes = [70, 35].map(delay => sample(Math.max(0, t - delay)));
  return { points, echoes };
}
