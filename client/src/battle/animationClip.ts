import type { BattleAnimationFrame } from "./animationTypes";

export function clipDurationMs(frames: BattleAnimationFrame[]) {
  return frames.reduce((sum, frame) => sum + frame.holdMs, 0);
}

export function clipFrameAt(frames: BattleAnimationFrame[], elapsedMs: number, playbackDurationMs = clipDurationMs(frames)) {
  if (!frames.length) throw new Error("Animation clip must define at least one frame");
  const sourceDurationMs = clipDurationMs(frames);
  if (sourceDurationMs <= 0) throw new Error("Animation clip frame durations must be positive");
  const scaledElapsedMs = playbackDurationMs > 0 ? Math.max(0, elapsedMs) * (sourceDurationMs / playbackDurationMs) : 0;
  // 缩放换算会留下 1e-12 量级的误差，加一点容差，保证正好落在帧边界时取到后一帧。
  let cursor = Math.min(scaledElapsedMs + 1e-6, Math.max(0, sourceDurationMs - Number.EPSILON));
  for (let index = 0; index < frames.length; index += 1) {
    const frame = frames[index];
    if (cursor < frame.holdMs || index === frames.length - 1) return { frame, index };
    cursor -= frame.holdMs;
  }
  return { frame: frames[frames.length - 1], index: frames.length - 1 };
}

export function clipImpactOffsetMs(frames: BattleAnimationFrame[], impactFrame: number, playbackDurationMs = clipDurationMs(frames)) {
  if (!Number.isInteger(impactFrame) || impactFrame < 0 || impactFrame >= frames.length) {
    throw new Error(`Animation impactFrame ${impactFrame} is outside the clip`);
  }
  const sourceDurationMs = clipDurationMs(frames);
  const sourceOffsetMs = frames.slice(0, impactFrame).reduce((sum, frame) => sum + frame.holdMs, 0);
  // 不取整：取整会让命中点落在出手帧之前一点点。
  return sourceDurationMs > 0 ? sourceOffsetMs * (playbackDurationMs / sourceDurationMs) : 0;
}

export function validateClipFrames(actionId: string, frames: BattleAnimationFrame[], durationMs: number, impactFrame: number) {
  if (!frames.length) throw new Error(`Animation action ${actionId} has no frames`);
  for (const frame of frames) {
    if (!frame.frameId || !Number.isFinite(frame.holdMs) || frame.holdMs <= 0) {
      throw new Error(`Animation action ${actionId} has an invalid frame`);
    }
  }
  const frameDurationMs = clipDurationMs(frames);
  if (frameDurationMs !== durationMs) {
    throw new Error(`Animation action ${actionId} durationMs=${durationMs} does not match frame holds=${frameDurationMs}`);
  }
  if (!Number.isInteger(impactFrame) || impactFrame < 0 || impactFrame >= frames.length) {
    throw new Error(`Animation action ${actionId} has invalid impactFrame=${impactFrame}`);
  }
}
