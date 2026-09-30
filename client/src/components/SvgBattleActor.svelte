<script lang="ts">
  import { SVG_SWORD_LENGTH, type SvgPose } from '../battle/svgBattlePose';
  import { bones } from '../battle/svgPoseLibrary';
  import type { CombatStyle, VisualProfile } from '../battle/animationTypes';
  export let pose: SvgPose;
  export let style: CombatStyle;
  export let profile: VisualProfile;
  export let flash = 0;
  /** 马尾末梢的外力偏移（人物自身朝向坐标，px）：由舞台按人物速度算出，冲刺向后扬、急停向前甩。 */
  export let hairFlow: number[] = [0, 0];
  /** 站着时的轻微摆动相位，取舞台时间，暂停和拖动时间轴都确定。 */
  export let hairPhase = 0;
  $: p = pose;

  type Pt = number[];
  /** 一个形状：路径或圆；fill 缺省为人物本色。 */
  type Shape = { d?: string; c?: [number, number, number]; fill?: string; opacity?: number };
  /** 一个部件：同一块形体的若干形状，整体描边、整体填色，所以部件内部没有接缝。 */
  type Part = { key: string; shapes: Shape[] };

  const sub = (a: Pt, b: Pt) => [a[0] - b[0], a[1] - b[1]];
  const len = (v: Pt) => Math.hypot(v[0], v[1]) || 1;
  const unit = (v: Pt) => { const l = len(v); return [v[0] / l, v[1] / l]; };
  const along = (a: Pt, u: Pt, k: number) => [a[0] + u[0] * k, a[1] + u[1] * k];
  const circle = (o: Pt, r: number): Shape => ({ c: [o[0], o[1], r] });

  /** 一段锥形肢体：两端各一个圆，中间用外公切线连起来，粗细过渡平滑、关节处自然是圆的。 */
  function taper(a: Pt, b: Pt, ra: number, rb: number) {
    const d = sub(b, a);
    const l = len(d);
    const u = [d[0] / l, d[1] / l];
    const n = [-u[1], u[0]];
    const s = Math.max(-0.95, Math.min(0.95, (ra - rb) / l));
    const c = Math.sqrt(1 - s * s);
    const m1 = [n[0] * c + u[0] * s, n[1] * c + u[1] * s];
    const m2 = [-n[0] * c + u[0] * s, -n[1] * c + u[1] * s];
    const P = (o: Pt, m: Pt, r: number) => `${o[0] + m[0] * r},${o[1] + m[1] * r}`;
    return `M${P(a, m1, ra)} L${P(b, m1, rb)} L${P(b, m2, rb)} L${P(a, m2, ra)} Z`;
  }
  const limb = (a: Pt, b: Pt, ra: number, rb: number): Shape[] => [{ d: taper(a, b, ra, rb) }, circle(a, ra), circle(b, rb)];

  /** 脚掌：落地时平放朝前，抬起时顺着小腿方向指出去。 */
  function footTip(knee: Pt, ankle: Pt) {
    const shin = unit(sub(ankle, knee));
    const w = Math.max(0, Math.min(1, shin[1]));
    return along(ankle, unit([w + shin[0] * (1 - w), shin[1] * (1 - w) + 0.12 * w]), 12);
  }

  /** 以 origin 为原点、按 deg 旋转后的多边形。剑用它画，不依赖 SVG transform，描边才能和别的形状一起算。 */
  function rotated(origin: Pt, deg: number, points: Pt[]) {
    const r = deg * Math.PI / 180, cs = Math.cos(r), sn = Math.sin(r);
    return 'M' + points.map(([x, y]) => `${origin[0] + x * cs - y * sn},${origin[1] + x * sn + y * cs}`).join(' L') + ' Z';
  }

  // 粗细（半径）：四肢从胸腔、骨盆两个体块里长出来——肩（三角肌）和大腿根做粗，与体块衔接成一整块；往末端收细。
  const R = { thigh: 10.5, knee: 7.2, ankle: 4.6, shoulder: 8.4, elbow: 5.8, wrist: 4.4, fist: 5.8, chest: 13.5, pelvis: 11.5, neck: 7.5 };
  const BLADE = '#f2ead2';

  /**
   * 马尾：发绳系在后脑，发束贴着后脑垂下、带一点 S 形，末梢被外力带着飘。
   * 一条主发束加一缕稍短的副发，都是两头收细的缎带。
   */
  function cubic(p0: Pt, p1: Pt, p2: Pt, p3: Pt, t: number) {
    const u = 1 - t;
    return [0, 1].map((i) => u * u * u * p0[i] + 3 * u * u * t * p1[i] + 3 * u * t * t * p2[i] + t * t * t * p3[i]);
  }
  function strand(p0: Pt, p1: Pt, p2: Pt, p3: Pt, root: number) {
    const n = 18;
    const pts = Array.from({ length: n + 1 }, (_, i) => cubic(p0, p1, p2, p3, i / n));
    const left: string[] = [], right: string[] = [];
    pts.forEach((pt, i) => {
      const a = pts[Math.max(0, i - 1)], b = pts[Math.min(n, i + 1)];
      const d = unit(sub(b, a));
      const t = i / n;
      // 根部饱满、中段略鼓、末梢收尖。
      const w = root * (1 - t) ** 0.45 * (1 + 0.4 * Math.sin(Math.PI * t * 0.8)) + 0.3;
      left.push(`${pt[0] - d[1] * w},${pt[1] + d[0] * w}`);
      right.push(`${pt[0] + d[1] * w},${pt[1] - d[0] * w}`);
    });
    return `M${left.join(" L")} L${right.reverse().join(" L")} Z`;
  }

  $: torsoAxis = unit(sub(p.shoulder, p.hip));
  $: chest = along(p.shoulder, torsoAxis, -8);
  $: frontToe = footTip(p.knee, p.foot);
  $: backToe = footTip(p.backKnee, p.backFoot);
  $: bladeDir = [Math.cos(p.blade * Math.PI / 180), Math.sin(p.blade * Math.PI / 180)];
  $: pommel = along(p.hand, bladeDir, -9);
  /** 剑穗从剑首垂下：方向取姿势里的 tassel 点，长度限制在一小段。 */
  $: tasselEnd = (() => {
    const v = sub(p.tassel, pommel);
    return along(pommel, unit(v), Math.min(len(v), 20));
  })();
  $: headUp = unit(sub(p.head, p.shoulder));
  $: headBackDir = [headUp[1], -headUp[0]];
  $: tie = along(along(p.head, headUp, bones.headRadius * 0.28), headBackDir, bones.headRadius * 0.9);
  $: sway = Math.sin(hairPhase / 260) * 2.5;
  $: flow = [hairFlow[0] + sway, hairFlow[1]];

  // 部件按由远到近排列：头发 → 后腿 → 后臂 → 躯干与头 → 前腿 → 前臂与剑。
  $: parts = ((): Part[] => {
    const list: Part[] = [];
    if (profile === 'female') {
      list.push({ key: 'hair', shapes: [
        { d: strand(along(tie, headBackDir, 1), [tie[0] + headBackDir[0] * 10, tie[1] + headBackDir[1] * 10 + 2],
          [tie[0] + headBackDir[0] * 16 + flow[0] * 0.7, tie[1] + headBackDir[1] * 16 + 14 + flow[1] * 0.7],
          [tie[0] + headBackDir[0] * 16 + flow[0] * 1.15, tie[1] + headBackDir[1] * 16 + 30 + flow[1] * 1.15], 2.4) },
        { d: strand(tie, [tie[0] + headBackDir[0] * 8, tie[1] + headBackDir[1] * 8 + 3],
          [tie[0] + headBackDir[0] * 11 + flow[0] * 0.5, tie[1] + headBackDir[1] * 11 + 20 + flow[1] * 0.5],
          [tie[0] + headBackDir[0] * 6 + flow[0], tie[1] + headBackDir[1] * 6 + 42 + flow[1]], 5) }
      ] });
    }
    list.push({ key: 'back-leg', shapes: [...limb(p.hip, p.backKnee, R.thigh, R.knee), ...limb(p.backKnee, p.backFoot, R.knee, R.ankle), ...limb(p.backFoot, backToe, R.ankle, 3)] });
    list.push({ key: 'back-arm', shapes: [...limb(p.shoulder, p.backElbow, R.shoulder, R.elbow), ...limb(p.backElbow, p.backHand, R.elbow, R.wrist), circle(p.backHand, R.fist)] });
    const body: Shape[] = [...limb(chest, p.hip, R.chest, R.pelvis), ...limb(p.shoulder, p.head, R.neck, R.neck), circle(p.head, bones.headRadius)];
    if (profile === 'female') body.push({ d: rotated(tie, Math.atan2(headBackDir[1], headBackDir[0]) * 180 / Math.PI, [[-2.4, -3.6], [2.4, -3.6], [2.4, 3.6], [-2.4, 3.6]]), fill: '#9c3b2e' });
    list.push({ key: 'body', shapes: body });
    list.push({ key: 'front-leg', shapes: [...limb(p.hip, p.knee, R.thigh, R.knee), ...limb(p.knee, p.foot, R.knee, R.ankle), ...limb(p.foot, frontToe, R.ankle, 3)] });
    const arm: Shape[] = [...limb(p.shoulder, p.elbow, R.shoulder, R.elbow), ...limb(p.elbow, p.hand, R.elbow, R.wrist)];
    if (style === 'sword') {
      const L = SVG_SWORD_LENGTH;
      arm.push({ d: taper(pommel, tasselEnd, 1.6, 1.2), opacity: 0.85 });
      arm.push({ d: rotated(p.hand, p.blade, [[-9, -2], [5, -2], [5, 2], [-9, 2]]) });
      arm.push({ d: rotated(p.hand, p.blade, [[4, -7], [7, -7], [7, 7], [4, 7]]) });
      arm.push({ d: rotated(p.hand, p.blade, [[7, -2], [L - 8, -2], [L, 0], [L - 8, 2], [7, 2]]), fill: BLADE });
    }
    arm.push(circle(p.hand, R.fist));
    list.push({ key: 'front-arm', shapes: arm });
    return list;
  })();
</script>

<svg class="vector-actor" viewBox="-128 -176 256 192" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
  <g fill="currentColor" style:filter={flash > 0 ? `brightness(${1 + flash * 0.35})` : undefined}>
    {#each parts as part (part.key)}
      <!-- 每个部件先整体描一圈深色边，再填色；后画的部件压在前面，边线就成了遮挡线。 -->
      <g class="part" data-part={part.key}>
        <g class="outline">
          {#each part.shapes as shape}
            {#if shape.c}<circle cx={shape.c[0]} cy={shape.c[1]} r={shape.c[2]} />{:else}<path d={shape.d} />{/if}
          {/each}
        </g>
        {#each part.shapes as shape}
          {#if shape.c}<circle cx={shape.c[0]} cy={shape.c[1]} r={shape.c[2]} fill={shape.fill} opacity={shape.opacity} />{:else}<path d={shape.d} fill={shape.fill} opacity={shape.opacity} />{/if}
        {/each}
      </g>
    {/each}
  </g>
</svg>

<style>
  .vector-actor { position: absolute; width: 256px; height: 192px; left: -128px; top: -176px; overflow: visible; }
  /* 描边只露在形体外沿：同色填充随后盖住内侧一半。颜色取舞台暗部，读作墨线。 */
  .outline { fill: none; stroke: #0d1714; stroke-width: 4.4px; stroke-linejoin: round; }
</style>
