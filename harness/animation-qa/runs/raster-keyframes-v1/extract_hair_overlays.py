#!/usr/bin/env python3
import argparse
import json
from pathlib import Path

from PIL import Image


def parse_args():
    parser = argparse.ArgumentParser(description="Extract generated cyan ponytails and align them to normalized body frames.")
    parser.add_argument("--body-source", required=True)
    parser.add_argument("--hair-source", required=True)
    parser.add_argument("--body-manifest", required=True)
    parser.add_argument("--body-frames", required=True)
    parser.add_argument("--out", required=True)
    parser.add_argument("--preview", required=True)
    return parser.parse_args()


def color_mask(image, kind):
    mask = Image.new("L", image.size, 0)
    source = image.convert("RGB").load()
    target = mask.load()
    for y in range(image.height):
        for x in range(image.width):
            r, g, b = source[x, y]
            if kind == "gold":
                target[x, y] = 255 if r > 145 and g > 95 and b < 115 and r > b * 1.6 else 0
            else:
                score = min(b - r, g - r)
                target[x, y] = max(0, min(255, (score - 18) * 4)) if b > 100 and g > 110 else 0
    return mask


def transformed_hair_cell(original_cell, edited_cell):
    original_gold_bbox = color_mask(original_cell, "gold").getbbox()
    edited_gold_bbox = color_mask(edited_cell, "gold").getbbox()
    cyan_mask = color_mask(edited_cell, "cyan")
    cyan_bbox = cyan_mask.getbbox()
    if not original_gold_bbox or not edited_gold_bbox or not cyan_bbox:
        raise ValueError("missing body or ponytail color mask")

    original_width = original_gold_bbox[2] - original_gold_bbox[0]
    original_height = original_gold_bbox[3] - original_gold_bbox[1]
    edited_width = edited_gold_bbox[2] - edited_gold_bbox[0]
    edited_height = edited_gold_bbox[3] - edited_gold_bbox[1]
    scale = (original_width / edited_width + original_height / edited_height) * 0.5

    cyan_crop = cyan_mask.crop(cyan_bbox)
    hair = Image.new("RGBA", cyan_crop.size, (255, 199, 25, 0))
    hair.putalpha(cyan_crop)
    out_width = max(1, round(hair.width * scale))
    out_height = max(1, round(hair.height * scale))
    hair = hair.resize((out_width, out_height), Image.Resampling.LANCZOS)

    paste_x = round(original_gold_bbox[0] + (cyan_bbox[0] - edited_gold_bbox[0]) * scale)
    paste_y = round(original_gold_bbox[1] + (cyan_bbox[1] - edited_gold_bbox[1]) * scale)
    layer = Image.new("RGBA", original_cell.size, (0, 0, 0, 0))
    layer.alpha_composite(hair, (paste_x, paste_y))
    return layer


def main():
    args = parse_args()
    body_source = Image.open(args.body_source).convert("RGBA")
    hair_source = Image.open(args.hair_source).convert("RGBA")
    manifest = json.loads(Path(args.body_manifest).read_text(encoding="utf-8"))
    out_dir = Path(args.out)
    out_dir.mkdir(parents=True, exist_ok=True)
    body_dir = Path(args.body_frames)
    target_width, target_height = manifest["target_size"]
    shared_scale = manifest["shared_scale"]
    previews = []

    for frame_data in manifest["frames"]:
        slot = frame_data["slot"]
        left, top, right, bottom = frame_data["source_cell"]
        original_cell = body_source.crop((left, top, right, bottom))
        edited_cell = hair_source.crop((left, top, right, bottom))
        hair_cell = transformed_hair_cell(original_cell, edited_cell)
        scaled = hair_cell.resize(
            (max(1, round(hair_cell.width * shared_scale)), max(1, round(hair_cell.height * shared_scale))),
            Image.Resampling.LANCZOS,
        )
        source_left, source_top, _, _ = frame_data["source_bbox"]
        paste_x, paste_y = frame_data["paste"]
        overlay = Image.new("RGBA", (target_width, target_height), (0, 0, 0, 0))
        overlay.alpha_composite(
            scaled,
            (round(paste_x - source_left * shared_scale), round(paste_y - source_top * shared_scale)),
        )
        overlay.save(out_dir / f"{slot}.png")

        body = Image.open(body_dir / f"{slot}.png").convert("RGBA")
        female = Image.new("RGBA", body.size, (0, 0, 0, 0))
        female.alpha_composite(overlay)
        female.alpha_composite(body)
        previews.append(female)

    preview = Image.new("RGBA", (len(previews) * target_width, target_height), (24, 27, 28, 255))
    for index, frame in enumerate(previews):
        preview.alpha_composite(frame, (index * target_width, 0))
    Path(args.preview).parent.mkdir(parents=True, exist_ok=True)
    preview.save(args.preview)
    print(json.dumps({"frames": len(previews), "out": str(out_dir)}, ensure_ascii=False))


if __name__ == "__main__":
    main()
