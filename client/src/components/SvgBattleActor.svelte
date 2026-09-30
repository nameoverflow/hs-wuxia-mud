<script context="module" lang="ts">
  let instances = 0;
</script>

<script lang="ts">
  import { SVG_SWORD_LENGTH, type SvgPose } from '../battle/svgBattlePose';
  import { ANKLE_HEIGHT, bones } from '../battle/svgPoseLibrary';
  import type { CombatStyle, VisualProfile } from '../battle/animationTypes';
  export let pose: SvgPose;
  export let style: CombatStyle;
  export let profile: VisualProfile;
  export let flash = 0;
  /** 马尾末梢的外力偏移（人物自身朝向坐标，px）：由舞台按人物速度算出，冲刺向后扬、急停向前甩。 */
  export let hairFlow: number[] = [0, 0];
  /** 站着时的轻微摆动相位，取舞台时间，暂停和拖动时间轴都确定。 */
  export let hairPhase = 0;
  /**
   * 女性造型：maiden 为发髻 + 发簪 + 披发、身形更纤细的少女女侠；ponytail 为此前的高马尾（对照用）。
   */
  export let look: 'maiden' | 'ponytail' = 'maiden';
  $: p = pose;

  type Pt = number[];
  /** 一个形状：路径或圆；fill 缺省为人物本色。 */
  type Shape = { d?: string; c?: [number, number, number]; fill?: string; opacity?: number };
  /**
   * 一个部件：同一块形体的若干形状，整体描边、整体填色，所以部件内部没有接缝。
   * joins：这个部件画完后要缝合的连接处——两块形体在连接点附近重新填一遍色，盖掉那里的描边。
   */
  type Join = { at: Pt; r: number; shapes: Shape[] };
  type Part = { key: string; shapes: Shape[]; joins: Join[] };
  /** 每个实例自己的 clipPath id 前缀，舞台上同时有双方和残影。 */
  const uid = `actor-${++instances}`;

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

  /**
   * 脚：踩地时脚掌平贴地面朝前，脚跟、脚尖都着地，站得稳；离地时顺着小腿方向指出去。
   */
  function foot(knee: Pt, ankle: Pt, planted: boolean): Shape[] {
    if (planted) {
      const ground = ankle[1] + ANKLE_HEIGHT;
      const heel = [ankle[0] - 4, ground - 2.8], toe = [ankle[0] + 12, ground - 2.3];
      return [...limb(ankle, [ankle[0] + 1, ground - 3], R.ankle, 3), ...limb(heel, toe, 2.8, 2.3)];
    }
    const shin = unit(sub(ankle, knee));
    return limb(ankle, along(ankle, unit([shin[0] * 0.6 + 0.4, shin[1] * 0.6]), 12), R.ankle, 3);
  }

  /** 以 origin 为原点、按 deg 旋转后的多边形。剑用它画，不依赖 SVG transform，描边才能和别的形状一起算。 */
  function rotated(origin: Pt, deg: number, points: Pt[]) {
    const r = deg * Math.PI / 180, cs = Math.cos(r), sn = Math.sin(r);
    return 'M' + points.map(([x, y]) => `${origin[0] + x * cs - y * sn},${origin[1] + x * sn + y * cs}`).join(' L') + ' Z';
  }

  // 粗细（半径）：四肢从胸腔、骨盆两个体块里长出来——肩（三角肌）和大腿根做粗，与体块衔接成一整块；往末端收细。
  const BODY = { thigh: 10.5, knee: 7.2, ankle: 4.6, shoulder: 8.4, elbow: 5.8, wrist: 4.4, fist: 5.8, chest: 13.5, pelvis: 11.5, neck: 9 };
  /** 少女体态：肩臂更细、胸腔更窄、腰更收、手更小，骨盆基本不变。 */
  const MAIDEN_BODY = { thigh: 9.6, knee: 6.5, ankle: 4.1, shoulder: 7, elbow: 4.9, wrist: 3.7, fist: 4.7, chest: 11.4, pelvis: 11, neck: 7.2 };
  $: maiden = profile === 'female' && look === 'maiden';
  $: R = maiden ? MAIDEN_BODY : BODY;
  /** 少女的头小一号，往颈根收一点，免得露出脖子。 */
  $: headR = maiden ? bones.headRadius - 1.2 : bones.headRadius;
  const BLADE = '#f2ead2';

  /**
   * 马尾：一条穿过若干控制点的平滑缎带（Catmull-Rom 样条），每个控制点有自己的半宽，
   * 所以能做出"发根收紧、翘起处饱满、垂下后收尖"的形状。
   */
  function ribbon(points: Pt[], widths: number[]) {
    const at = (i: number) => points[Math.max(0, Math.min(points.length - 1, i))];
    const center: Pt[] = [], half: number[] = [];
    for (let i = 0; i < points.length - 1; i++) {
      const [p0, p1, p2, p3] = [at(i - 1), at(i), at(i + 1), at(i + 2)];
      for (let k = 0; k < 8; k++) {
        const t = k / 8, t2 = t * t, t3 = t2 * t;
        center.push([0, 1].map((j) => 0.5 * (2 * p1[j] + (p2[j] - p0[j]) * t + (2 * p0[j] - 5 * p1[j] + 4 * p2[j] - p3[j]) * t2 + (3 * p1[j] - p0[j] - 3 * p2[j] + p3[j]) * t3)));
        half.push(widths[i] + (widths[i + 1] - widths[i]) * t);
      }
    }
    center.push(points[points.length - 1]);
    half.push(widths[widths.length - 1]);
    const left: string[] = [], right: string[] = [];
    center.forEach((pt, i) => {
      const d = unit(sub(center[Math.min(center.length - 1, i + 1)], center[Math.max(0, i - 1)]));
      left.push(`${pt[0] - d[1] * half[i]},${pt[1] + d[0] * half[i]}`);
      right.push(`${pt[0] + d[1] * half[i]},${pt[1] - d[0] * half[i]}`);
    });
    return `M${left.join(" L")} L${right.reverse().join(" L")} Z`;
  }

  $: torsoAxis = unit(sub(p.neck, p.hip));
  $: chest = along(p.neck, torsoAxis, -10);
  $: bladeDir = [Math.cos(p.blade * Math.PI / 180), Math.sin(p.blade * Math.PI / 180)];
  $: pommel = along(p.hand, bladeDir, -9);
  /** 剑穗从剑首垂下：方向取姿势里的 tassel 点，长度限制在一小段。 */
  $: tasselEnd = (() => {
    const v = sub(p.tassel, pommel);
    return along(pommel, unit(v), Math.min(len(v), 20));
  })();
  $: headUp = unit(sub(p.head, p.neck));
  $: headBackDir = [headUp[1], -headUp[0]];
  $: headC = maiden ? along(p.head, headUp, -1.2) : p.head;
  /** 马尾：在后脑偏上束起，发根向后微微翘起一小截，再垂到腰。 */
  $: root = along(along(p.head, headUp, bones.headRadius * 0.5), headBackDir, bones.headRadius * 0.62);
  $: flow = hairFlow;
  /** 摆动沿发束向下传：越靠发梢摆得越大、越晚。两个频率叠加，不像节拍器。 */
  const swing = (phase: number, lag: number) => Math.sin(phase / 300 - lag) * 1 + Math.sin(phase / 170 - lag * 1.6) * 0.4;
  $: sway1 = swing(hairPhase, 0.4) * 2.5;
  $: sway2 = swing(hairPhase, 1.1) * 6;

  // 部件按由远到近排列：头发 → 后腿 → 后臂 → 躯干与头 → 前腿 → 前臂与剑。
  $: parts = ((): Part[] => {
    const list: Part[] = [];
    const shapesOf = (key: string) => list.find((part) => part.key === key)?.shapes ?? [];
    /** 自然相连处（肩、髋、发根）只描外轮廓：画完较近的那块后，在连接点附近把两块的填色重铺一遍。 */
    const join = (key: string, others: string[], at: Pt, r: number) => {
      const part = list.find((candidate) => candidate.key === key);
      if (part) part.joins.push({ at, r, shapes: [...others.flatMap(shapesOf), ...part.shapes] });
    };
    // 少女女侠：后脑挽发髻、平插发簪，发髻下披散几缕长短不一的长发。
    const up = headUp, back = headBackDir;
    const onFace = (u: number, b: number): Pt => [headC[0] + up[0] * headR * u + back[0] * headR * b, headC[1] + up[1] * headR * u + back[1] * headR * b];
    // 发髻从后脑上方鼓出来；发簪几乎平着向后穿过发髻，只微微上翘。
    const bun = onFace(0.5, 0.98), bunTop = onFace(0.82, 0.72);
    const pinDir = unit([up[0] * 0.3 + back[0] * 0.95, up[1] * 0.3 + back[1] * 0.95]);
    const looseRoot = onFace(0.05, 0.86);
    if (maiden) {
      /** 垂下的发丝：以后脑为根，b 向后、down 向下；外力和摆动越往下越大。 */
      const h = (b: number, down: number, f: number, sw: number): Pt =>
        [looseRoot[0] + back[0] * (b + sw) + flow[0] * f, looseRoot[1] + back[1] * (b + sw) + down + flow[1] * f];
      list.push({ key: 'hair', joins: [], shapes: [
        { d: ribbon([h(8, 8, 0.2, 0), h(14, 18, 0.9, sway1), h(19, 28, 1.5, sway1 * 1.5)], [1.6, 1.2, 0.3]) },
        { d: ribbon([along(looseRoot, back, 2), h(10, 14, 0.3, sway1 * 0.5), h(15, 38, 1.1, sway1 * 1.2), h(13, 55, 1.8, sway2 * 1.3)], [2.8, 2.8, 2, 0.3]) },
        { d: ribbon([h(1, 4, 0, 0), h(3, 26, 0.5, sway1 * 0.8), h(1, 50, 1.2, sway2 * 0.9), h(-2, 68, 1.7, sway2 * 1.1)], [2.2, 2, 1.4, 0.3]) },
        { d: ribbon([looseRoot, h(6, 10, 0.2, 0), h(9, 32, 0.8, sway1), h(5, 61, 1.5, sway2)], [4.2, 4.6, 3.4, 0.4]) },
        // 发簪：平穿发髻，簪头从前面露一点，簪尾向后伸出、收尖。
        { d: taper(along(bun, pinDir, -9), along(bun, pinDir, 15), 1.5, 0.6) },
        circle(bun, 6.8),
        circle(bunTop, 4.6)
      ] });
    } else if (profile === 'female') {
      // 发根和翘起的那一截跟着头走；垂下的部分受重力，外力（冲刺、急停、受击）和摆动越往下越大。
      const up = headUp, back = headBackDir;
      const onHead = (u: number, b: number) => [root[0] + up[0] * u + back[0] * b, root[1] + up[1] * u + back[1] * b];
      const hanging = (b: number, down: number, f: number, sw: number) =>
        [root[0] + back[0] * b + flow[0] * f + sw, root[1] + back[1] * b + down + flow[1] * f];
      const apex = onHead(4, 9);
      list.push({ key: 'hair', joins: [], shapes: [
        // 发梢分出的一缕：从中段分叉，尖朝外翻。
        { d: ribbon([hanging(22, 14, 0.45, sway1), hanging(27, 36, 1, sway1 * 1.2), hanging(26, 54, 1.7, sway2 * 1.25)], [3.4, 2.6, 0.5]) },
        { d: ribbon([onHead(-2, -1), apex, hanging(21, 8, 0.3, sway1 * 0.4), hanging(22, 34, 0.9, sway1), hanging(15, 64, 1.6, sway2)], [3.6, 5.2, 6.4, 5, 0.6]) },
        circle(onHead(1, 1), 4.6)
      ] });
    }
    list.push({ key: 'back-leg', joins: [], shapes: [...limb(p.hip, p.backKnee, R.thigh, R.knee), ...limb(p.backKnee, p.backFoot, R.knee, R.ankle), ...foot(p.backKnee, p.backFoot, p.backFootPlanted)] });
    list.push({ key: 'back-arm', joins: [], shapes: [...limb(p.shoulder, p.backElbow, R.shoulder, R.elbow), ...limb(p.backElbow, p.backHand, R.elbow, R.wrist), circle(p.backHand, R.fist)] });
    const body: Shape[] = [...limb(chest, p.hip, R.chest, R.pelvis), ...limb(p.neck, headC, R.neck, R.neck), circle(headC, headR)];
    list.push({ key: 'body', joins: [], shapes: body });
    join('body', ['back-leg'], p.hip, R.thigh + 10);
    join('body', ['back-arm'], p.shoulder, R.shoulder + 8);
    if (maiden) {
      join('body', ['hair'], looseRoot, 10);
      join('body', ['hair'], bun, 8);
    } else if (profile === 'female') join('body', ['hair'], root, 8);
    list.push({ key: 'front-leg', joins: [], shapes: [...limb(p.hip, p.knee, R.thigh, R.knee), ...limb(p.knee, p.foot, R.knee, R.ankle), ...foot(p.knee, p.foot, p.footPlanted)] });
    const arm: Shape[] = [...limb(p.shoulder, p.elbow, R.shoulder, R.elbow), ...limb(p.elbow, p.hand, R.elbow, R.wrist)];
    if (style === 'sword') {
      const L = SVG_SWORD_LENGTH;
      arm.push({ d: taper(pommel, tasselEnd, 1.6, 1.2), opacity: 0.85 });
      arm.push({ d: rotated(p.hand, p.blade, [[-9, -2], [5, -2], [5, 2], [-9, 2]]) });
      arm.push({ d: rotated(p.hand, p.blade, [[4, -7], [7, -7], [7, 7], [4, 7]]) });
      arm.push({ d: rotated(p.hand, p.blade, [[7, -2], [L - 8, -2], [L, 0], [L - 8, 2], [7, 2]]), fill: BLADE });
    }
    arm.push(circle(p.hand, R.fist));
    list.push({ key: 'front-arm', joins: [], shapes: arm });
    // 两腿在骨盆处相连、两臂在肩处相连，也算自然连接。
    join('front-leg', ['back-leg', 'body'], p.hip, R.thigh + 10);
    join('front-arm', ['back-arm', 'body'], p.shoulder, R.shoulder + 8);
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
      {#each part.joins as join, j}
        <!-- 缝合：只在连接点附近、只在两块形体内部重铺填色，外轮廓的描边保留，接缝处的描边被盖掉。 -->
        <clipPath id={`${uid}-${part.key}-${j}`}><circle cx={join.at[0]} cy={join.at[1]} r={join.r} /></clipPath>
        <g clip-path={`url(#${uid}-${part.key}-${j})`}>
          {#each join.shapes as shape}
            {#if shape.c}<circle cx={shape.c[0]} cy={shape.c[1]} r={shape.c[2]} fill={shape.fill} opacity={shape.opacity} />{:else}<path d={shape.d} fill={shape.fill} opacity={shape.opacity} />{/if}
          {/each}
        </g>
      {/each}
    {/each}
  </g>
</svg>

<style>
  .vector-actor { position: absolute; width: 256px; height: 192px; left: -128px; top: -176px; overflow: visible; }
  /* 描边只露在形体外沿：同色填充随后盖住内侧一半。颜色取舞台暗部，读作墨线。 */
  .outline { fill: none; stroke: #0d1714; stroke-width: 4.4px; stroke-linejoin: round; }
</style>
