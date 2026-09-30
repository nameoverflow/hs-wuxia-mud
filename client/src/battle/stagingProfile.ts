import stagingData from "../../../resources/scripts/combat_presentation/staging.json";
import type { BattleChoreography, StagingOverride, StagingProfile } from "./animationTypes";

type Preset = StagingOverride & { extends?: string };
const library = stagingData as unknown as { schemaVersion: number; presets: Record<string, Preset> };

const isObject = (value: unknown): value is Record<string, unknown> => !!value && typeof value === "object" && !Array.isArray(value);

/** 深合并：对象逐层覆盖，数值直接替换。 */
export function mergeStaging<T>(base: T, override: unknown): T {
  if (!isObject(override)) return base;
  const out: Record<string, unknown> = { ...(base as Record<string, unknown>) };
  for (const [key, value] of Object.entries(override)) {
    if (key === "extends") continue;
    out[key] = isObject(value) && isObject(out[key]) ? mergeStaging(out[key], value) : value;
  }
  return out as T;
}

const resolved = new Map<string, StagingProfile>();

export function stagingPreset(name: string, trail: string[] = []): StagingProfile {
  const cached = resolved.get(name);
  if (cached) return cached;
  const preset = library.presets[name];
  if (!preset) throw new Error(`Unknown staging preset ${name}`);
  if (trail.includes(name)) throw new Error(`Staging preset cycle: ${[...trail, name].join(" → ")}`);
  const base = preset.extends ? stagingPreset(preset.extends, [...trail, name]) : ({} as StagingProfile);
  const profile = mergeStaging(base, preset);
  resolved.set(name, profile);
  return profile;
}

export function stagingFor(weight: BattleChoreography["weight"], ...overrides: (StagingOverride | undefined)[]): StagingProfile {
  return overrides.reduce<StagingProfile>((profile, override) => mergeStaging(profile, override), stagingPreset(weight));
}

if (library.schemaVersion !== 1) throw new Error(`Unsupported staging schema ${library.schemaVersion}`);
for (const weight of ["light", "heavy", "quiet"] as const) {
  const profile = stagingPreset(weight);
  const required = [profile.force, profile.shade, profile.tilt, profile.flash, profile.fallMs, profile.camera?.kick, profile.camera?.zoom,
    profile.trail?.leadMs, profile.reactions?.hit?.push, profile.reactions?.dodge?.leadMs, profile.reactions?.parry?.standoff];
  if (!required.every(Number.isFinite)) throw new Error(`Staging preset ${weight} is incomplete`);
}
