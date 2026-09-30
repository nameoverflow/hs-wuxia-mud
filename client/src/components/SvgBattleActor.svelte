<script lang="ts">
  import { SVG_SWORD_LENGTH, type SvgPose } from '../battle/svgBattlePose';
  import type { CombatStyle, VisualProfile } from '../battle/animationTypes';
  export let pose: SvgPose;
  export let style: CombatStyle;
  export let profile: VisualProfile;
  export let flash = 0;
  $: p = pose;
  const limb = (a: number[], b: number[], c: number[]) => `M${a} L${b} L${c}`;
  /** 衣袂与剑穗各一笔墨线，末梢直接取关键姿势里的固定点，不做模拟。 */
  const sweep = (from: number[], to: number[], bulge = 8, sag = 0) =>
    `M${from} Q${(from[0] + to[0]) / 2 + bulge},${(from[1] + to[1]) / 2 + sag} ${to}`;
</script>

<svg class="vector-actor" viewBox="-128 -176 256 192" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
  <g fill="currentColor" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round">
    <!-- 衣袂先画，压在身体后面，甩向动作的反方向。 -->
    <path d={sweep(p.hip, p.robe, -10, 6)} fill="none" stroke-width="13" opacity="0.5" />
    {#if profile === 'female'}
      <path d={`M${p.head[0]-15},${p.head[1]-8} C${p.head[0]-32},${p.head[1]-5} ${p.head[0]-18},${p.head[1]+16} ${p.head[0]-31},${p.head[1]+27} S${p.head[0]-32},${p.head[1]+44} ${p.head[0]-39},${p.head[1]+47}`} fill="none" stroke-width="7" />
    {/if}
    <path d={limb(p.hip, p.backKnee, p.backFoot)} fill="none" stroke-width="19" />
    <path d={`M${p.backFoot} l8,0`} fill="none" stroke-width="11" />
    <path d={limb(p.shoulder, p.backElbow, p.backHand)} fill="none" stroke-width="16" />
    <circle cx={p.backHand[0]} cy={p.backHand[1]} r="8" stroke="none" />
    <path d={limb(p.hip, p.knee, p.foot)} fill="none" stroke-width="19" />
    <path d={`M${p.foot} l8,0`} fill="none" stroke-width="11" />
    <path d={`M${p.shoulder[0]-12},${p.shoulder[1]} Q${p.shoulder[0]},${p.shoulder[1]-10} ${p.shoulder[0]+13},${p.shoulder[1]} Q${p.shoulder[0]+20},${p.shoulder[1]+15} ${p.hip[0]+13},${p.hip[1]+3} Q${p.hip[0]},${p.hip[1]+14} ${p.hip[0]-13},${p.hip[1]+3} Q${p.shoulder[0]-18},${p.shoulder[1]+14} ${p.shoulder[0]-12},${p.shoulder[1]} Z`} stroke="none" />
    <path d={`M${p.shoulder} L${p.head}`} stroke-width="12" />
    <ellipse cx={p.head[0]} cy={p.head[1]} rx="19" ry="20" stroke="none" />
    <path d={limb(p.shoulder, p.elbow, p.hand)} fill="none" stroke-width="16" />
    <circle cx={p.hand[0]} cy={p.hand[1]} r="8" stroke="none" />
    {#if style === 'sword'}
      <!-- 剑穗：从剑柄垂向末梢，略微下坠。 -->
      <path d={sweep(p.hand, p.tassel, 4, 10)} fill="none" stroke-width="4" opacity="0.85" />
      <g transform={`translate(${p.hand}) rotate(${p.blade})`}>
        <path d="M-9,0 H5" stroke-width="4" />
        <path d="M5,-7 V7" stroke-width="3" />
        <path d={`M7,-2 L${SVG_SWORD_LENGTH-8},-2 ${SVG_SWORD_LENGTH},0 ${SVG_SWORD_LENGTH-8},2 7,2 Z`} stroke="none" fill="#f2ead2" />
      </g>
    {/if}
  </g>
  {#if flash > 0}<circle cx={p.shoulder[0]+7} cy={p.shoulder[1]+22} r="17" fill="#fff4d9" opacity={flash} />{/if}
</svg>

<style>
  .vector-actor { position: absolute; width: 256px; height: 192px; left: -128px; top: -176px; overflow: visible; }
</style>
