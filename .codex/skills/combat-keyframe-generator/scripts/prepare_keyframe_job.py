#!/usr/bin/env python3
import argparse
import json
from pathlib import Path
from textwrap import dedent


DEFAULT_STYLE = (
    "simplified compact abstract wuxia little-person silhouette sprite for a dark text-MUD battle UI; "
    "match docs/assets/battle-animation/actor-style-reference-male.png or actor-style-reference-female.png proportions; "
    "squat compact 2.5-3 head-tall body, low center of gravity, broad readable action shapes; "
    "male and female share the same total height, modest softened head size, torso scale, limb thickness, and foot baseline; "
    "front and rear arms have balanced thickness and visual weight, with the rear waist-side arm reading as a compact bent arm; "
    "must still read as a small person with head above torso, readable arms and legs, and planted feet; "
    "flat small-game actor, 1-2 colors, no facial features, no clothing, no hair strands, no gradients"
)
DEFAULT_FRAMES = "idle,windup,strike,impact,recover"


PROFILE_CARDS = {
    "neutral": (
        "Neutral small abstract combat actor; compact plain little-person silhouette, simple body mass, minimal identity markers, "
        "same flat style and stable scale across all frames"
    ),
    "male": (
        "Male baseline actor; plain softened rounded head, simple torso, balanced compact front/rear guard arms, simple legs, no hair, no clothing; slightly squarer stance is acceptable; "
        "add a slim straight sword only when the action requires a sword"
    ),
    "female": (
        "Female baseline actor; same simple body family as male with balanced compact front/rear guard arms, plus one solid high-tied long ponytail reaching below the waist line toward the upper hip along the back, with a flowing S-curve and tapered or subtly split tip, no clothing; slightly lighter, narrower, more upright stance is acceptable; "
        "add a slim straight sword only when the action requires a sword"
    ),
    "custom": "",
}


FRAME_POSES = {
    "idle": "balanced combat stance, weapon readable but not at maximum extension, feet planted",
    "windup": "small reverse preparation before force, lowered center, weapon and shoulders preparing",
    "strike": "maximum readable action extension, clear line of force, full body visible",
    "impact": "contact pose near maximum reach, leave space for VFX at weapon tip or target side",
    "recover": "settling back toward neutral stance, clean and readable",
    "target_hurt": "defender recoils visibly from impact while staying in frame",
    "target_parry": "defender holds a stable guard or raised weapon intercepting force",
    "target_dodge": "defender leans or steps aside, afterimage-friendly, avoiding the original attack line",
}


def parse_args():
    parser = argparse.ArgumentParser(description="Prepare an image_gen keyframe job for hs-wuxia-mud combat animation.")
    parser.add_argument("--out", required=True, help="Output job folder.")
    parser.add_argument("--name", required=True, help="Stable job/action name, e.g. cold-rain-thrust.")
    parser.add_argument("--action", required=True, help="Natural-language action description.")
    parser.add_argument("--actor", required=True, help="Stable actor/character card.")
    parser.add_argument(
        "--identity-profile",
        default="neutral",
        choices=sorted(PROFILE_CARDS.keys()),
        help="Reusable actor profile to append to the actor card.",
    )
    parser.add_argument("--frames", default=DEFAULT_FRAMES, help=f"Comma-separated frame slots. Default: {DEFAULT_FRAMES}")
    parser.add_argument("--style", default=DEFAULT_STYLE, help="Visual style card.")
    parser.add_argument("--direction", default="right", choices=["right", "left"], help="Facing direction for generated frames.")
    parser.add_argument("--background", default="#00ff00", help="Flat chroma-key background color.")
    parser.add_argument("--target-size", default="256x192", help="Final project frame size.")
    return parser.parse_args()


def frame_prompt(args, slot):
    pose = FRAME_POSES.get(slot, f"pose that clearly expresses the {slot} keyframe for this action")
    actor_card = resolved_actor_card(args)
    return dedent(
        f"""
        Create exactly one 2D game animation keyframe.
        Project: Wuxia text-MUD battle UI.
        Frame slot: {slot}.
        Action: {args.action}
        Actor: {actor_card}
        Style: {args.style}.
        Canvas: 4:3 frame intended for final {args.target_size} RGBA PNG.
        Direction: side-view, facing {args.direction}.
        Reference: match the compact proportions, low center of gravity, and abstraction level of the correct project reference: docs/assets/battle-animation/actor-style-reference-male.png for male actors or docs/assets/battle-animation/actor-style-reference-female.png for female actors.
        Background: perfectly flat solid {args.background} chroma key for removal, no scenery.
        Pose: {pose}.
        Hard constraints: one full-body actor only; feet visible; feet on the same horizontal baseline; no crop; no cast shadow; no motion blur; no text; no labels; no UI; no frame border; no grid; no background details; no attack trail, sword glow, spark, damage number, or VFX baked into the actor frame; same body shape, same gender marker, same weapon state, same proportions as the reference.
        Simplicity constraints: flat 1-2 color silhouette; no facial features; no eyes; no mouth; no clothing, no robe, no sleeves, no belt, no boots; no hair strands; no ornate accessories; no gradients; no painterly texture; no internal line art except one or two necessary cutout gaps for pose readability.
        """
    ).strip()


def resolved_actor_card(args):
    profile = PROFILE_CARDS.get(args.identity_profile, "")
    if args.identity_profile == "custom" or not profile:
        return args.actor
    return f"{args.actor}. {profile}"


def main():
    args = parse_args()
    out = Path(args.out)
    out.mkdir(parents=True, exist_ok=True)
    (out / "generated").mkdir(exist_ok=True)
    (out / "restyled").mkdir(exist_ok=True)
    (out / "accepted").mkdir(exist_ok=True)
    slots = [slot.strip() for slot in args.frames.split(",") if slot.strip()]

    manifest = {
        "name": args.name,
        "action": args.action,
        "actor": args.actor,
        "identity_profile": args.identity_profile,
        "resolved_actor_card": resolved_actor_card(args),
        "style": args.style,
        "direction": args.direction,
        "background": args.background,
        "target_size": args.target_size,
        "frames": [{"slot": slot, "filename": f"{slot}.png"} for slot in slots],
        "final_asset_root": "client/src/assets/battle/actors/",
    }
    (out / "manifest.json").write_text(json.dumps(manifest, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")

    actor_card = resolved_actor_card(args)
    anchor_prompt = dedent(
        f"""
        Create a clean anchor reference for a 2D game combat sprite.
        Project: Wuxia text-MUD battle UI.
        Actor: {actor_card}
        Style: {args.style}.
        Canvas: 4:3 frame intended for final {args.target_size} RGBA PNG.
        Direction: side-view, facing {args.direction}.
        Reference: match the compact proportions, low center of gravity, and abstraction level of the correct project reference: docs/assets/battle-animation/actor-style-reference-male.png for male actors or docs/assets/battle-animation/actor-style-reference-female.png for female actors.
        Background: perfectly flat solid {args.background} chroma key for removal, no scenery.
        Pose: readable neutral combat stance, feet planted on a stable baseline.
        Hard constraints: one full-body actor only; feet visible; no crop; no cast shadow; no motion blur; no text; no labels; no UI; no frame border; no grid; no background details; no attack trail, sword glow, spark, damage number, or VFX baked into the actor frame.
        Simplicity constraints: flat 1-2 color silhouette; no facial features; no eyes; no mouth; no clothing, no robe, no sleeves, no belt, no boots; no hair strands; no ornate accessories; no gradients; no painterly texture; no internal line art except one or two necessary cutout gaps for pose readability.
        """
    ).strip()

    prompts = ["# Keyframe Generation Prompts", "", "## Anchor", "", anchor_prompt, ""]
    queue = [{"kind": "anchor", "slot": "anchor", "prompt": anchor_prompt}]
    for slot in slots:
        prompt = frame_prompt(args, slot)
        prompts.extend([f"## Frame: {slot}", "", prompt, ""])
        queue.append({"kind": "frame", "slot": slot, "filename": f"{slot}.png", "prompt": prompt})

    (out / "prompts.md").write_text("\n".join(prompts), encoding="utf-8")
    with (out / "generation_queue.jsonl").open("w", encoding="utf-8") as handle:
        for item in queue:
            handle.write(json.dumps(item, ensure_ascii=False) + "\n")

    checklist = dedent(
        f"""
        # Job Checklist

        1. Read `.codex/skills/combat-keyframe-generator/references/frame-format.md`.
        2. Inspect `docs/assets/battle-animation/actor-style-reference-male.png` or `docs/assets/battle-animation/actor-style-reference-female.png`; use the correct image as the visual reference for body proportions and gender marker.
        3. Keep weapon family and visual identity separated: do not mix sword, fist, male, female, or common reactions in one runtime pool.
        4. Generate a job anchor with built-in image_gen only if a new actor family is needed.
        5. Save selected source images under `{out}/generated/`.
        6. Inspect with view_image.
        7. Gate action and style separately. If action passes but style fails, use the v13 anchor as identity/style reference and the draft as pose-only reference, then save the corrected frame under `{out}/restyled/`.
        8. Reject and regenerate any source where the actor or weapon touches the source sheet edge, or a source cell edge when using grid mode.
        9. Normalize final accepted frames to `{args.target_size}` RGBA PNG with `scripts/normalize_keyframes.py` or the same shared-scale algorithm. Never resize each frame independently to its own bbox height.
        10. Save accepted frames under `{out}/accepted/`.
        11. Copy final project assets under `client/src/assets/battle/actors/...`.
        12. If animation motion matters, wire frames and run `$animation-visual-qa`.
        """
    ).strip()
    (out / "checklist.md").write_text(checklist + "\n", encoding="utf-8")

    print(out.resolve())


if __name__ == "__main__":
    main()
