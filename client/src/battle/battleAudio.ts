import { writable } from "svelte/store";
import type { ResolvedBattleTimeline } from "./animationTypes";

// Audio is opt-in. Context creation/resume occurs only in a user gesture.
export const battleSoundEnabled = writable(false);
let enabled = false;
let context: AudioContext | null = null;

export async function toggleBattleSound() {
  if (enabled) { enabled = false; battleSoundEnabled.set(false); return; }
  try {
    context ??= new AudioContext();
    await context.resume();
    enabled = context.state === "running";
    battleSoundEnabled.set(enabled);
  } catch { enabled = false; battleSoundEnabled.set(false); }
}

export function playBattleImpact(timeline: ResolvedBattleTimeline) {
  if (!enabled || !context || context.state !== "running" || timeline.kind === "effect_tick") return;
  const ctx = context;
  const now = ctx.currentTime;
  const oscillator = ctx.createOscillator();
  const gain = ctx.createGain();
  const healing = !!timeline.heal || timeline.actor.motion === "focus";
  const parry = timeline.result === "parry";
  const dodge = timeline.result === "dodge";
  const heavy = timeline.choreography.weight === "heavy";
  const duration = healing ? 0.24 : parry ? 0.16 : 0.09;
  oscillator.type = parry ? "triangle" : "sine";
  oscillator.frequency.setValueAtTime(healing ? 440 : parry ? 1100 : dodge ? 240 : heavy ? 100 : 150, now);
  oscillator.frequency.exponentialRampToValueAtTime(healing ? 660 : parry ? 430 : 45, now + duration);
  gain.gain.setValueAtTime(0, now);
  gain.gain.linearRampToValueAtTime(dodge ? 0.018 : 0.055, now + 0.004);
  gain.gain.exponentialRampToValueAtTime(0.0001, now + duration);
  oscillator.connect(gain).connect(ctx.destination);
  oscillator.start(now);
  oscillator.stop(now + duration + 0.02);
  oscillator.onended = () => { oscillator.disconnect(); gain.disconnect(); };
}
