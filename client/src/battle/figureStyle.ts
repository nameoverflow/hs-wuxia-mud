import { get, writable } from "svelte/store";

/**
 * 剪影的可选画法，用来并排对比：
 * - ink：flat 为等宽描边；brush 为粗细有变化的笔触墨线 + 自上而下的明暗。
 * - proportion：classic 为现在的比例；tall 为头小一号、腿长一截的修长比例。
 * - look：女性造型，maiden 为发髻披发，ponytail 为高马尾。
 */
export interface FigureStyle {
  ink: "flat" | "brush";
  proportion: "classic" | "tall";
  look: "maiden" | "ponytail";
}

export const figureStyle = writable<FigureStyle>({ ink: "flat", proportion: "classic", look: "maiden" });

export function currentFigureStyle() {
  return get(figureStyle);
}
