import { writable } from "svelte/store";
import type { ResolvedBattleTimeline } from "./animationTypes";

// Audio is opt-in. Context creation/resume occurs only in a user gesture.
export const battleSoundEnabled = writable(false);
let enabled = false;
let context: AudioContext | null = null;
let noise: AudioBuffer | null = null;

export async function toggleBattleSound() {
  if (enabled) { enabled = false; battleSoundEnabled.set(false); return; }
  try {
    context ??= new AudioContext();
    await context.resume();
    enabled = context.state === "running";
    battleSoundEnabled.set(enabled);
  } catch { enabled = false; battleSoundEnabled.set(false); }
}

/** 一段可复用的白噪声，用来做闷响和破风，比正弦波更像打击。 */
function noiseBuffer(ctx: AudioContext) {
  if (noise) return noise;
  const frames = Math.floor(ctx.sampleRate * 0.5);
  noise = ctx.createBuffer(1, frames, ctx.sampleRate);
  const data = noise.getChannelData(0);
  for (let i = 0; i < frames; i += 1) data[i] = Math.random() * 2 - 1;
  return noise;
}

interface Voice {
  /** 噪声层的带通中心频率，决定"闷"还是"脆"。 */
  center: number;
  q: number;
  /** 低频冲击的频率落点，给体积感。 */
  thump: number;
  gain: number;
  decay: number;
  /** 金属泛音，招架用。 */
  ring?: number;
}

export function playBattleImpact(timeline: ResolvedBattleTimeline) {
  if (!enabled || !context || context.state !== "running" || timeline.kind === "effect_tick") return;
  const ctx = context;
  const now = ctx.currentTime;
  const heavy = timeline.choreography.weight === "heavy";
  const healing = !!timeline.heal || timeline.actor.motion === "focus";
  const voice: Voice =
    timeline.result === "parry" ? { center: 2600, q: 1.4, thump: 180, gain: 0.09, decay: 0.34, ring: 2100 }
    : timeline.result === "dodge" ? { center: 900, q: 2.4, thump: 90, gain: 0.03, decay: 0.2 }
    : healing ? { center: 620, q: 0.9, thump: 160, gain: 0.05, decay: 0.5 }
    : heavy ? { center: 380, q: 0.8, thump: 70, gain: 0.16, decay: 0.42 }
    : { center: 900, q: 1.1, thump: 130, gain: 0.12, decay: 0.22 };

  const master = ctx.createGain();
  master.gain.setValueAtTime(0, now);
  master.gain.linearRampToValueAtTime(voice.gain, now + 0.004);
  master.gain.exponentialRampToValueAtTime(0.0001, now + voice.decay);
  master.connect(ctx.destination);

  const source = ctx.createBufferSource();
  source.buffer = noiseBuffer(ctx);
  const band = ctx.createBiquadFilter();
  band.type = "bandpass";
  band.frequency.setValueAtTime(voice.center, now);
  band.frequency.exponentialRampToValueAtTime(Math.max(80, voice.center * 0.45), now + voice.decay);
  band.Q.value = voice.q;
  source.connect(band).connect(master);
  source.start(now);
  source.stop(now + voice.decay + 0.02);

  const body = ctx.createOscillator();
  const bodyGain = ctx.createGain();
  body.type = "sine";
  body.frequency.setValueAtTime(voice.thump * 2, now);
  body.frequency.exponentialRampToValueAtTime(voice.thump * 0.55, now + voice.decay * 0.7);
  bodyGain.gain.setValueAtTime(0.7, now);
  bodyGain.gain.exponentialRampToValueAtTime(0.0001, now + voice.decay * 0.8);
  body.connect(bodyGain).connect(master);
  body.start(now);
  body.stop(now + voice.decay + 0.02);

  if (voice.ring) {
    const ring = ctx.createOscillator();
    const ringGain = ctx.createGain();
    ring.type = "triangle";
    ring.frequency.setValueAtTime(voice.ring, now);
    ringGain.gain.setValueAtTime(0.28, now);
    ringGain.gain.exponentialRampToValueAtTime(0.0001, now + voice.decay);
    ring.connect(ringGain).connect(master);
    ring.start(now);
    ring.stop(now + voice.decay + 0.02);
  }

  source.onended = () => { master.disconnect(); band.disconnect(); bodyGain.disconnect(); };
}

/** 起手破风。在蓄势转出招的那一刻响，比命中早一点。 */
export function playBattleWhoosh(timeline: ResolvedBattleTimeline) {
  if (!enabled || !context || context.state !== "running" || timeline.kind === "effect_tick") return;
  if (!["approach", "lunge", "drive"].includes(timeline.actor.motion)) return;
  const ctx = context;
  const now = ctx.currentTime;
  const duration = 0.3;
  const source = ctx.createBufferSource();
  source.buffer = noiseBuffer(ctx);
  const band = ctx.createBiquadFilter();
  band.type = "bandpass";
  band.frequency.setValueAtTime(320, now);
  band.frequency.exponentialRampToValueAtTime(1900, now + duration * 0.75);
  band.Q.value = 0.7;
  const gain = ctx.createGain();
  gain.gain.setValueAtTime(0.0001, now);
  gain.gain.linearRampToValueAtTime(0.05, now + duration * 0.6);
  gain.gain.exponentialRampToValueAtTime(0.0001, now + duration);
  source.connect(band).connect(gain).connect(ctx.destination);
  source.start(now);
  source.stop(now + duration + 0.02);
  source.onended = () => { gain.disconnect(); band.disconnect(); };
}
