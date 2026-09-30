import { readdirSync, readFileSync } from "node:fs";
import path from "node:path";

/**
 * Battle actions live in one or more manifests under resources/scripts/combat_actions/
 * (the Haskell server loads the same directory). Each file is { schemaVersion, actions }.
 */
export function readBattleActions(repoRoot) {
  const dir = path.join(repoRoot, "resources/scripts/combat_actions");
  const files = readdirSync(dir).filter((name) => name.endsWith(".json")).sort();
  const manifests = files.map((file) => ({ file, ...JSON.parse(readFileSync(path.join(dir, file), "utf8")) }));
  return {
    manifests,
    actions: manifests.flatMap((manifest) => (manifest.actions || []).map((action) => ({ ...action, sourceFile: manifest.file })))
  };
}
