<script context="module" lang="ts">
  let instances = 0;
</script>

<script lang="ts">
  import { SVG_SWORD_LENGTH, type SvgPose } from '../battle/svgBattlePose';
  import { ANKLE_HEIGHT, type HandShape } from '../battle/svgPoseLibrary';
  import type { CombatStyle, VisualProfile } from '../battle/animationTypes';
  import type { FigureStyle } from '../battle/figureStyle';
  export let pose: SvgPose;
  export let style: CombatStyle;
  export let profile: VisualProfile;
  export let flash = 0;
  /** 马尾末梢的外力偏移（人物自身朝向坐标，px）：由舞台按人物速度算出，冲刺向后扬、急停向前甩。 */
  export let hairFlow: number[] = [0, 0];
  /** 站着时的轻微摆动相位，取舞台时间，暂停和拖动时间轴都确定。 */
  export let hairPhase = 0;
  /** 女性造型：maiden 为发髻 + 发簪 + 披发、身形更纤细的少女女侠；ponytail 为此前的高马尾（对照用）。 */
  export let look: FigureStyle['look'] = 'maiden';
  /** 描边：flat 为等宽描边；brush 为粗细有变化、带手绘抖动的笔触墨线，加自上而下的明暗。 */
  export let ink: FigureStyle['ink'] = 'flat';
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
  /** 每个实例自己的 id 前缀（clipPath、滤镜、渐变），舞台上同时有双方和残影。 */
  const uid = `actor-${++instances}`;

  const sub = (a: Pt, b: Pt) => [a[0] - b[0], a[1] - b[1]];
  const len = (v: Pt) => Math.hypot(v[0], v[1]) || 1;
  const unit = (v: Pt) => { const l = len(v); return [v[0] / l, v[1] / l]; };
  const along = (a: Pt, u: Pt, k: number) => [a[0] + u[0] * k, a[1] + u[1] * k];
  const rot = (v: Pt, deg: number) => { const r = deg * Math.PI / 180, c = Math.cos(r), s = Math.sin(r); return [v[0] * c - v[1] * s, v[0] * s + v[1] * c]; };
  const circle = (o: Pt, r: number): Shape => ({ c: [o[0], o[1], r] });
  const smooth = (t: number) => t * t * (3 - 2 * t);

  /** 一段锥形：两端各一个圆，中间用外公切线连起来。 */
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

  /** 闭合的平滑曲线（Catmull-Rom 转三次贝塞尔），手、脚这些小形状用它，拐角都是圆的。 */
  function closedCurve(points: Pt[]) {
    const n = points.length, at = (i: number) => points[(i + n) % n];
    let d = `M${points[0]}`;
    for (let i = 0; i < n; i++) {
      const [p0, p1, p2, p3] = [at(i - 1), at(i), at(i + 1), at(i + 2)];
      d += ` C${p1[0] + (p2[0] - p0[0]) / 6},${p1[1] + (p2[1] - p0[1]) / 6} ${p2[0] - (p3[0] - p1[0]) / 6},${p2[1] - (p3[1] - p1[1]) / 6} ${p2}`;
    }
    return d + ' Z';
  }

  /**
   * 一条穿过若干控制点的平滑缎带（Catmull-Rom 样条），两侧各有一组半宽，
   * 所以同一段肢体可以一侧鼓（小腿肚、大腿前侧）一侧平。
   */
  function ribbon(points: Pt[], left: number[], right: number[] = left) {
    const at = (i: number) => points[Math.max(0, Math.min(points.length - 1, i))];
    const center: Pt[] = [], hl: number[] = [], hr: number[] = [];
    for (let i = 0; i < points.length - 1; i++) {
      const [p0, p1, p2, p3] = [at(i - 1), at(i), at(i + 1), at(i + 2)];
      for (let k = 0; k < 8; k++) {
        const t = k / 8, t2 = t * t, t3 = t2 * t, e = smooth(t);
        center.push([0, 1].map((j) => 0.5 * (2 * p1[j] + (p2[j] - p0[j]) * t + (2 * p0[j] - 5 * p1[j] + 4 * p2[j] - p3[j]) * t2 + (3 * p1[j] - p0[j] - 3 * p2[j] + p3[j]) * t3)));
        hl.push(left[i] + (left[i + 1] - left[i]) * e);
        hr.push(right[i] + (right[i + 1] - right[i]) * e);
      }
    }
    center.push(points[points.length - 1]);
    hl.push(left[left.length - 1]);
    hr.push(right[right.length - 1]);
    const L: string[] = [], Rt: string[] = [];
    center.forEach((pt, i) => {
      const d = unit(sub(center[Math.min(center.length - 1, i + 1)], center[Math.max(0, i - 1)]));
      L.push(`${pt[0] - d[1] * hl[i]},${pt[1] + d[0] * hl[i]}`);
      Rt.push(`${pt[0] + d[1] * hr[i]},${pt[1] - d[0] * hr[i]}`);
    });
    return `M${L.join(" L")} L${Rt.reverse().join(" L")} Z`;
  }

  /**
   * 一段有肌肉起伏的肢体：沿骨头取几个点，每个点给左右半宽（左侧是骨头方向转 +90° 的一侧，
   * 骨头朝下时就是身后），两端补圆，关节处与相邻一段自然衔接。
   */
  type Profile = { t: number[]; l: number[]; r: number[] };
  function organic(a: Pt, b: Pt, prof: Profile, k: number): Shape[] {
    const pts = prof.t.map((t) => [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t]);
    const l = prof.l.map((w) => w * k), r = prof.r.map((w) => w * k);
    return [{ d: ribbon(pts, l, r) }, circle(a, Math.max(l[0], r[0])), circle(b, Math.min(l[l.length - 1], r[r.length - 1]))];
  }
  // 起伏：大腿前侧饱满，小腿肚在后侧，上臂近肩饱满，前臂近肘略鼓；都往腕、踝收细。
  const THIGH: Profile = { t: [0, 0.4, 1], l: [10.2, 9.4, 6.8], r: [10.2, 10.8, 6.8] };
  const SHIN: Profile = { t: [0, 0.32, 0.78, 1], l: [6.8, 8.1, 5, 4.1], r: [6.8, 6.4, 4.6, 4.1] };
  const UPPER_ARM: Profile = { t: [0, 0.35, 1], l: [8.2, 7.2, 5.4], r: [8.2, 7.4, 5.4] };
  const FOREARM: Profile = { t: [0, 0.3, 1], l: [5.6, 6, 3.7], r: [5.6, 5.7, 3.7] };

  /** 手：握拳是方圆的拳形；立掌是扁平的掌，手腕微微上翘；剑指是并拢伸出的两指。 */
  function hand(elbow: Pt, wrist: Pt, shape: HandShape, k: number): Shape[] {
    const dir = unit(sub(wrist, elbow));
    // “上翘”取远离地面的那一侧，前臂朝下时就是朝前。
    const upSide = rot(dir, -90)[1] <= rot(dir, 90)[1] ? -1 : 1;
    const bend = (deg: number) => rot(dir, deg * upSide);
    if (shape === 'finger') {
      const f = bend(12);
      return [circle(along(wrist, dir, 2), 3.6 * k), { d: taper(along(wrist, dir, 2), along(wrist, f, 13 * k), 2.1 * k, 1.3 * k) }, circle(along(wrist, f, 13 * k), 1.3 * k)];
    }
    if (shape === 'palm') {
      const f = bend(28), n = rot(f, 90 * upSide);
      const base = along(wrist, dir, 1.5), tip = along(base, f, 11 * k);
      return [
        { d: closedCurve([along(base, n, 3.4 * k), along(along(base, f, 6 * k), n, 3.4 * k), along(tip, n, 2.3 * k), along(tip, f, 1.8 * k), along(tip, n, -2.3 * k), along(along(base, f, 6 * k), n, -3.2 * k), along(base, n, -3.2 * k)]) },
        { d: taper(along(base, f, 2 * k), along(along(base, f, 4 * k), n, 5 * k), 1.8 * k, 1.3 * k) }
      ];
    }
    const c = along(wrist, dir, 3.6 * k), n = rot(dir, 90);
    const w = 4.8 * k, h = 4.6 * k;
    return [{ d: closedCurve([along(along(c, dir, -h), n, w * 0.8), along(along(c, dir, h * 0.7), n, w), along(along(c, dir, h), n, 0), along(along(c, dir, h * 0.7), n, -w), along(along(c, dir, -h), n, -w * 0.8)]) }];
  }

  /** 脚：鞋形，有脚跟、脚背和收尖的脚尖。踩地时平贴地面朝前；离地时顺着小腿绷直。 */
  function foot(knee: Pt, ankle: Pt, planted: boolean, k: number): Shape[] {
    const f = planted ? [1, 0] : unit(rot(unit(sub(ankle, knee)), -18));
    const up = [f[1], -f[0]];
    const g = along(ankle, up, -ANKLE_HEIGHT);
    const P = (x: number, h: number) => [g[0] + f[0] * x * k + up[0] * h, g[1] + f[1] * x * k + up[1] * h];
    return [circle(ankle, SHIN.l[3] * k), { d: closedCurve([P(-5, 0.6), P(-5.6, 3.4), P(-3.6, 7), P(2.6, 6.6), P(8, 3.6), P(13, 1.5), P(13.8, 0.4), P(4, -0.2)]) }];
  }

  /** 以 origin 为原点、按 deg 旋转后的多边形。剑用它画，不依赖 SVG transform，描边才能和别的形状一起算。 */
  function rotated(origin: Pt, deg: number, points: Pt[]) {
    const r = deg * Math.PI / 180, cs = Math.cos(r), sn = Math.sin(r);
    return 'M' + points.map(([x, y]) => `${origin[0] + x * cs - y * sn},${origin[1] + x * sn + y * cs}`).join(' L') + ' Z';
  }

  const BLADE = '#f2ead2';

  $: maiden = profile === 'female' && look === 'maiden';
  /** 少女体态：四肢整体细一号，胸腔更窄、腰更收，骨盆基本不变。 */
  $: k = maiden ? 0.86 : 1;
  $: headR = p.bones.headRadius - (maiden ? 1.2 : 0);
  $: torsoAxis = unit(sub(p.neck, p.hip));
  $: chest = along(p.neck, torsoAxis, -10);
  $: waist = along(p.hip, torsoAxis, 14);
  $: bladeDir = [Math.cos(p.blade * Math.PI / 180), Math.sin(p.blade * Math.PI / 180)];
  $: pommel = along(p.hand, bladeDir, -9);
  /** 剑穗从剑首垂下：方向取姿势里的 tassel 点，长度限制在一小段。 */
  $: tasselEnd = (() => {
    const v = sub(p.tassel, pommel);
    return along(pommel, unit(v), Math.min(len(v), 20));
  })();
  $: headUp = unit(sub(p.head, p.neck));
  $: headBackDir = [headUp[1], -headUp[0]];
  /** 少女的头小一号，往颈根收一点，免得露出脖子。 */
  $: headC = maiden ? along(p.head, headUp, -1.2) : p.head;
  /** 马尾：在后脑偏上束起，发根向后微微翘起一小截，再垂到腰。 */
  $: root = along(along(p.head, headUp, headR * 0.5), headBackDir, headR * 0.62);
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
    const up = headUp, back = headBackDir;
    const onFace = (u: number, b: number): Pt => [headC[0] + up[0] * headR * u + back[0] * headR * b, headC[1] + up[1] * headR * u + back[1] * headR * b];
    // 少女女侠：后脑挽发髻、平插发簪，发髻下披散几缕长短不一的长发。
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
      const onHead = (u: number, b: number) => [root[0] + up[0] * u + back[0] * b, root[1] + up[1] * u + back[1] * b];
      const hanging = (b: number, down: number, f: number, sw: number) =>
        [root[0] + back[0] * b + flow[0] * f + sw, root[1] + back[1] * b + down + flow[1] * f];
      const apex = onHead(4, 9);
      list.push({ key: 'hair', joins: [], shapes: [
        { d: ribbon([hanging(22, 14, 0.45, sway1), hanging(27, 36, 1, sway1 * 1.2), hanging(26, 54, 1.7, sway2 * 1.25)], [3.4, 2.6, 0.5]) },
        { d: ribbon([onHead(-2, -1), apex, hanging(21, 8, 0.3, sway1 * 0.4), hanging(22, 34, 0.9, sway1), hanging(15, 64, 1.6, sway2)], [3.6, 5.2, 6.4, 5, 0.6]) },
        circle(onHead(1, 1), 4.6)
      ] });
    }
    list.push({ key: 'back-leg', joins: [], shapes: [...organic(p.hip, p.backKnee, THIGH, k), ...organic(p.backKnee, p.backFoot, SHIN, k), ...foot(p.backKnee, p.backFoot, p.backFootPlanted, k)] });
    list.push({ key: 'back-arm', joins: [], shapes: [...organic(p.shoulder, p.backElbow, UPPER_ARM, k), ...organic(p.backElbow, p.backHand, FOREARM, k), ...hand(p.backElbow, p.backHand, p.hands[1], k)] });
    // 躯干：胸腔 → 收腰 → 骨盆，一条连续的形体。
    const chestR = maiden ? 11.4 : 13.5, waistR = maiden ? 8.6 : 10.6, pelvisR = maiden ? 11 : 11.5;
    const body: Shape[] = [
      { d: ribbon([chest, waist, p.hip], [chestR, waistR, pelvisR]) }, circle(chest, chestR), circle(p.hip, pelvisR),
      { d: taper(p.neck, headC, 8 * k, 8 * k) }, circle(headC, headR)
    ];
    list.push({ key: 'body', joins: [], shapes: body });
    join('body', ['back-leg'], p.hip, 10.5 * k + 10);
    join('body', ['back-arm'], p.shoulder, 8.4 * k + 8);
    if (maiden) {
      join('body', ['hair'], looseRoot, 10);
      join('body', ['hair'], bun, 8);
    } else if (profile === 'female') join('body', ['hair'], root, 8);
    list.push({ key: 'front-leg', joins: [], shapes: [...organic(p.hip, p.knee, THIGH, k), ...organic(p.knee, p.foot, SHIN, k), ...foot(p.knee, p.foot, p.footPlanted, k)] });
    const arm: Shape[] = [...organic(p.shoulder, p.elbow, UPPER_ARM, k), ...organic(p.elbow, p.hand, FOREARM, k)];
    if (style === 'sword') {
      const L = SVG_SWORD_LENGTH;
      arm.push({ d: taper(pommel, tasselEnd, 1.6, 1.2), opacity: 0.85 });
      arm.push({ d: rotated(p.hand, p.blade, [[-9, -2], [5, -2], [5, 2], [-9, 2]]) });
      arm.push({ d: rotated(p.hand, p.blade, [[4, -7], [7, -7], [7, 7], [4, 7]]) });
      arm.push({ d: rotated(p.hand, p.blade, [[7, -2], [L - 8, -2], [L, 0], [L - 8, 2], [7, 2]]), fill: BLADE });
    }
    // 持剑的手握着剑柄，拳画在剑柄上面。
    arm.push(...hand(p.elbow, p.hand, style === 'sword' ? 'fist' : p.hands[0], k));
    list.push({ key: 'front-arm', joins: [], shapes: arm });
    // 两腿在骨盆处相连、两臂在肩处相连，也算自然连接。
    join('front-leg', ['back-leg', 'body'], p.hip, 10.5 * k + 10);
    join('front-arm', ['back-arm', 'body'], p.shoulder, 8.4 * k + 8);
    return list;
  })();
  $: skin = ink === 'brush' ? `url(#${uid}-shade)` : 'currentColor';
</script>

<svg class="vector-actor" viewBox="-128 -176 256 192" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
  {#if ink === 'brush'}
    <defs>
      <!-- 笔触墨线：整体外扩一圈，再朝后下方多扩一截，所以背光的下沿和后侧更粗；再加一点手绘抖动。 -->
      <filter id={`${uid}-brush`} x="-30%" y="-30%" width="160%" height="160%">
        <feMorphology in="SourceAlpha" operator="dilate" radius="1" result="thin" />
        <feOffset in="SourceAlpha" dx="-1.3" dy="1.5" result="shifted" />
        <feMorphology in="shifted" operator="dilate" radius="1.5" result="heavy" />
        <feMerge result="stroke"><feMergeNode in="thin" /><feMergeNode in="heavy" /></feMerge>
        <feTurbulence type="fractalNoise" baseFrequency="0.22" numOctaves="2" seed="7" result="noise" />
        <feDisplacementMap in="stroke" in2="noise" scale="1.6" xChannelSelector="R" yChannelSelector="G" result="wobbly" />
        <feFlood flood-color="#0d1714" />
        <feComposite in2="wobbly" operator="in" />
      </filter>
      <!-- 明暗：头肩略亮、腿脚略暗，整体一个渐变，不把身体切成几块。 -->
      <linearGradient id={`${uid}-shade`} gradientUnits="userSpaceOnUse" x1="0" y1="-150" x2="0" y2="0">
        <stop offset="0" style:stop-color="color-mix(in srgb, currentColor 86%, white)" />
        <stop offset="0.55" style:stop-color="currentColor" />
        <stop offset="1" style:stop-color="color-mix(in srgb, currentColor 78%, black)" />
      </linearGradient>
    </defs>
  {/if}
  <g fill={skin} style:filter={flash > 0 ? `brightness(${1 + flash * 0.35})` : undefined}>
    {#each parts as part (part.key)}
      <!-- 每个部件先整体描一圈深色边，再填色；后画的部件压在前面，边线就成了遮挡线。 -->
      <g class="part" data-part={part.key}>
        {#if ink === 'brush'}
          <g filter={`url(#${uid}-brush)`}>
            {#each part.shapes as shape}
              {#if shape.c}<circle cx={shape.c[0]} cy={shape.c[1]} r={shape.c[2]} />{:else}<path d={shape.d} />{/if}
            {/each}
          </g>
        {:else}
          <g class="outline">
            {#each part.shapes as shape}
              {#if shape.c}<circle cx={shape.c[0]} cy={shape.c[1]} r={shape.c[2]} />{:else}<path d={shape.d} />{/if}
            {/each}
          </g>
        {/if}
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
