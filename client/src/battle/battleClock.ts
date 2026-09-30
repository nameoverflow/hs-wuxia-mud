import { writable } from "svelte/store";
import type { ResolvedBattleTimeline } from "./animationTypes";

export interface BattlePlayback {
  id: number | null;
  elapsedMs: number;
  durationMs: number;
  paused: boolean;
}

interface ClockDriver {
  now: () => number;
  request: (callback: (now: number) => void) => number;
  cancel: (id: number) => void;
}

const browserDriver: ClockDriver = {
  now: () => performance.now(),
  request: (callback) => requestAnimationFrame(callback),
  cancel: (id) => cancelAnimationFrame(id)
};

/** The queue, sound, health presentation and stage all observe this one clock. */
export class BattleClock {
  readonly state = writable<BattlePlayback>({ id: null, elapsedMs: 0, durationMs: 0, paused: false });
  private timeline: ResolvedBattleTimeline | null = null;
  private elapsed = 0;
  private previous = 0;
  private frame: number | null = null;
  private paused = false;
  private suspended = false;
  private speed = 1;
  /** 下一段待触发的命中；拖动时间轴不会让已触发的命中重来。 */
  private nextHit = 0;
  private generation = 0;
  private onImpact: (hitIndex: number) => void = () => {};
  private onComplete: () => void = () => {};

  constructor(private driver: ClockDriver = browserDriver) {}

  play(timeline: ResolvedBattleTimeline, onImpact: (hitIndex: number) => void, onComplete: () => void) {
    this.cancel();
    this.timeline = timeline;
    this.onImpact = onImpact;
    this.onComplete = onComplete;
    this.previous = this.driver.now();
    this.emit();
    this.frame = this.driver.request(this.tick);
  }

  pause(paused = true) {
    this.paused = paused;
    this.previous = this.driver.now();
    this.emit();
  }

  setSpeed(speed: number) {
    this.speed = Math.max(0.1, Math.min(3, Number.isFinite(speed) ? speed : 1));
    this.previous = this.driver.now();
  }

  suspend(suspended: boolean) {
    this.suspended = suspended;
    this.previous = this.driver.now();
    this.emit();
  }

  /** Scrubbing is visual-only: callbacks cannot be replayed by seeking backwards. */
  seek(elapsedMs: number) {
    if (!this.timeline) return;
    this.elapsed = Math.max(0, Math.min(this.timeline.durationMs, elapsedMs));
    this.previous = this.driver.now();
    this.emit();
  }

  cancel() {
    this.generation += 1;
    if (this.frame !== null) this.driver.cancel(this.frame);
    this.frame = null;
    this.timeline = null;
    this.elapsed = 0;
    this.nextHit = 0;
    this.paused = false;
    this.emit();
  }

  private emit() {
    this.state.set({ id: this.timeline?.id ?? null, elapsedMs: this.elapsed, durationMs: this.timeline?.durationMs ?? 0, paused: this.paused || this.suspended });
  }

  private tick = (now: number) => {
    this.frame = null;
    const timeline = this.timeline;
    const generation = this.generation;
    if (!timeline) return;
    const stopped = this.paused || this.suspended;
    if (!stopped) this.elapsed = Math.min(timeline.durationMs, this.elapsed + Math.max(0, now - this.previous) * this.speed);
    this.previous = now;
    this.emit();
    while (!stopped && this.nextHit < timeline.hits.length && this.elapsed >= timeline.hits[this.nextHit].atMs) {
      this.onImpact(this.nextHit++);
      if (this.generation !== generation) return;
    }
    if (!stopped && this.elapsed >= timeline.durationMs) {
      this.timeline = null;
      this.onComplete();
      return;
    }
    this.frame = this.driver.request(this.tick);
  };
}

export const battleClock = new BattleClock();
export const battlePlayback = battleClock.state;
