export const apMax = 100;
export const baselineAgility = 19;
export const targetActionSeconds = 2;
export const apGainPerAgilitySecond = apMax / (targetActionSeconds * baselineAgility);

export function normalizeAgility(value: number) {
  return Math.max(1, Number.isFinite(value) ? value : baselineAgility);
}

export function apSecondsUntil(value: number, target: number, agility: number, max = apMax) {
  const remaining = Math.max(0, target - value) * max;
  return remaining / (normalizeAgility(agility) * apGainPerAgilitySecond);
}

export function predictApValue(base: number, agility: number, syncedAt: number, now: number, max = apMax, cap = max) {
  const safeBase = Math.max(0, Math.min(max, Number.isFinite(base) ? base : 0));
  const elapsedSeconds = Math.max(0, (now - syncedAt) / 1000);
  const gained = elapsedSeconds * normalizeAgility(agility) * apGainPerAgilitySecond;
  return Math.min(cap, safeBase + gained);
}
