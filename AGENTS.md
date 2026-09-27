# AGENTS.md

## Project and task routing

Wuxia MUD with a Haskell/Stack WebSocket server, YAML game content, and a
Svelte/TypeScript web client. Player persistence uses JSON saves.

Read the documentation relevant to the change; a full documentation pass is
not required. [docs/README.md](docs/README.md) indexes the rest.

| Task | Reference |
| --- | --- |
| Start server/client, development fixtures | [docs/running.md](docs/running.md) |
| State boundaries and threading | [docs/architecture.md](docs/architecture.md), [docs/tick-loop-and-heartbeat.md](docs/tick-loop-and-heartbeat.md) |
| YAML content and quests | [docs/content-scripting.md](docs/content-scripting.md) |
| Combat and progression | [docs/gameplay-systems.md](docs/gameplay-systems.md), [docs/character-progression.md](docs/character-progression.md) |
| Protocol or player saves | [docs/protocol-and-persistence.md](docs/protocol-and-persistence.md) |
| Client UI | [docs/client-ui.md](docs/client-ui.md) |
| Animation integration | [docs/battle-animation.md](docs/battle-animation.md) |
| Animation appearance and motion | [docs/combat-animation-design-guide.md](docs/combat-animation-design-guide.md) |
| Feature planning | [docs/status-and-roadmap.md](docs/status-and-roadmap.md); verify relevant claims against current code |

## Commands and completion

Run backend commands from the repository root; client commands use `npm --prefix client`.

| Change or task | Relevant command / evidence |
| --- | --- |
| Build / run backend | `stack build` / `stack exec mud-hs-exe` (127.0.0.1:9160) |
| Backend gameplay, parsing, persistence | `stack test` |
| Client types and Svelte components | `npm --prefix client run check` |
| Client production bundle | `npm --prefix client run build` |
| Animation catalog and asset references | `npm --prefix client run validate:animations` |
| Timeline, clock, battle presentation logic | `npm --prefix client run test:battle` |
| Browser battle interaction | `npm --prefix client run test:battle:browser` |
| Visual animation change | Record the relevant running scene and inspect it with `animation-visual-qa` |

Select checks for the changed behavior. `check` and `build` already include
animation validation; avoid redundant runs unless something changed or failed.
Documentation-only edits need link/command/consistency checks, not a full game build.

Within the requested scope, continue through implementation, relevant verification,
and fixes for failures caused by the change without asking for approval at each
local step. For visual work, completion includes inspecting the running result;
a successful build alone does not establish visual quality. Report what was
verified and any unresolved limitation. Do not expand a review-only request into edits.

For full-game development, `scripts/dev-test.sh` starts both processes and normally
resets the named test player's save. Use a dedicated test user or `--no-reset` to
preserve an existing save. The battle lab provides client fixtures for animation
work without needing a gameplay save.

## Implementation constraints

- Keep `allow-newer: true` in `stack.yaml` for the project's GHC compatibility.
- Follow existing lens and monad-transformer patterns. Combat updates `Battle`;
  it does not directly mutate `GameState`.
- Use `GameException`, `CombatException`, or `WorldException` and `throwError`
  in the corresponding layer rather than `error`.
- Preserve exception-safe MVar updates; never acquire the same MVar recursively.
- Use `Data.Aeson.KeyMap` and `Data.Aeson.Key` for Aeson objects, not `HashMap`.
- Qualify ambiguous names such as `Prelude.show`; import `unless` from `Control.Monad`.
- Use YAML for content supported by the existing schema; extend parsing and world
  validation when adding new content semantics.

## Image Asset Generation Policy

When creating or modifying visual game assets, especially character sprites,
animation frames, skill effects, icons, and style reference images, use the
Codex `imagegen` skill / `image_gen` tool as the source of the actual artwork.

Do **not** manually create final artwork with SVG path tracing, hand-authored
vector shapes, PIL/canvas geometry drawing, pixel-by-pixel edits, or other
code-native/manual drawing techniques. Do not "fix" an image by redrawing the
character or effect manually unless the user explicitly asks for a code-native
vector/SVG asset.

Allowed deterministic post-processing is limited to clearly mechanical steps
that do not invent or redraw the artwork: copying/moving generated files,
cropping, resizing, padding, spritesheet slicing/assembly, format conversion,
alpha/chroma-key removal, palette cleanup, compression, and alignment against
an existing generated frame. If a visual change affects the shape, pose,
style, anatomy, silhouette, costume, weapon, VFX design, or perceived art
direction, perform it through `image_gen` generation/editing instead of manual
editing.

If `image_gen` cannot produce the needed asset, stop and report the limitation
or ask before using a fallback workflow.

## Agent Harness Artifacts

Use `harness/` for agent-generated material that should remain available for
future QA, prompt reuse, or design decisions. Do not create new top-level
`reports/` or `tmp/` directories for agent work.

Current layout:

- `harness/animation-qa/runs/` - review prompts, QA verdicts, bounding boxes,
  and other lightweight records from local animation checks.
- `harness/animation-qa/references/` - reusable reference clips and analysis
  notes for animation timing or visual style.
- `harness/portrait-generation/jobs/` - prompt histories, QA manifests, notes,
  and contact sheets for portrait-generation jobs.
- `harness/portrait-generation/candidates/` - reusable generated candidates
  that are not final in-game assets or documentation assets yet.
- `harness/tmp/` - ignored runtime scratch space for recordings, storyboards,
  crop tests, generated lists, markers, and other files that can be regenerated.

Commit files that preserve a decision, a repeatable prompt, a source reference,
or a reusable generated candidate. Put pure execution byproducts under
`harness/tmp/` so they stay local. If a generated visual becomes a final game
asset, move it into the appropriate `client/src/assets/` or `docs/assets/`
location and keep only the supporting prompt/QA notes in `harness/`.
