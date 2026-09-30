import { get, writable } from "svelte/store";

/**
 * 剪影的可选画法，用来并排对比：
 * - look：女性造型，maiden 为发髻披发，ponytail 为高马尾。
 */
export interface FigureStyle {
  look: "maiden" | "ponytail";
}

export const figureStyle = writable<FigureStyle>({ look: "maiden" });

export function currentFigureStyle() {
  return get(figureStyle);
}
