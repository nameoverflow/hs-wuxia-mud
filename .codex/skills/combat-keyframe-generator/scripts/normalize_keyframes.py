#!/usr/bin/env python3
import argparse
import json
from collections import deque
from pathlib import Path

from PIL import Image, ImageDraw


def parse_size(value):
    try:
        width, height = value.lower().split("x", 1)
        return int(width), int(height)
    except ValueError as exc:
        raise argparse.ArgumentTypeError("size must be WIDTHxHEIGHT") from exc


def parse_hex_color(value):
    raw = value.strip().lstrip("#")
    if len(raw) != 6:
        raise argparse.ArgumentTypeError("color must be #rrggbb")
    try:
        return tuple(int(raw[i : i + 2], 16) for i in (0, 2, 4))
    except ValueError as exc:
        raise argparse.ArgumentTypeError("color must be #rrggbb") from exc


def parse_args():
    parser = argparse.ArgumentParser(
        description="Normalize generated combat keyframe sheets to project-ready RGBA frames."
    )
    parser.add_argument("--source", required=True, help="Generated source sheet PNG.")
    parser.add_argument("--out", required=True, help="Accepted output directory.")
    parser.add_argument("--project-out", help="Optional project asset directory to mirror outputs into.")
    parser.add_argument("--slots", required=True, help="Comma-separated slot names in row-major order.")
    parser.add_argument("--cols", type=int, required=True, help="Number of sheet columns.")
    parser.add_argument("--rows", type=int, default=1, help="Number of sheet rows.")
    parser.add_argument(
        "--mode",
        choices=["components", "grid"],
        default="components",
        help="components segments the whole sheet by alpha blobs; grid crops fixed cells.",
    )
    parser.add_argument("--target-size", type=parse_size, default=(256, 192), help="Final size, default 256x192.")
    parser.add_argument("--baseline", type=int, default=176, help="Final foot baseline y coordinate.")
    parser.add_argument("--top-margin", type=int, default=8, help="Minimum top margin after scaling.")
    parser.add_argument("--side-margin", type=int, default=8, help="Minimum side margin after scaling.")
    parser.add_argument("--reference-slot", default="idle", help="Slot used to choose shared scale.")
    parser.add_argument("--target-reference-height", type=int, default=146, help="Output height for the reference slot.")
    parser.add_argument("--chroma-key", type=parse_hex_color, default=(0, 255, 0), help="Flat source background color.")
    parser.add_argument("--edge-margin", type=int, default=4, help="Fail if subject touches a source cell edge this closely.")
    parser.add_argument("--component-threshold", type=float, default=0.02, help="Keep components at least this fraction of the largest component.")
    parser.add_argument("--cleanup-threshold", type=float, default=0.04, help="Drop tiny fragments inside each final pose crop.")
    parser.add_argument("--preview", help="Optional preview PNG path.")
    parser.add_argument("--manifest", help="Optional manifest JSON path.")
    return parser.parse_args()


def remove_chroma(image, key):
    image = image.convert("RGBA")
    pixels = image.load()
    width, height = image.size
    kr, kg, kb = key
    for y in range(height):
        for x in range(width):
            r, g, b, a = pixels[x, y]
            distance = abs(r - kr) + abs(g - kg) + abs(b - kb)
            green_like = g > 80 and g > r * 1.35 and g > b * 1.35
            key_like = distance < 130 or green_like
            if key_like:
                pixels[x, y] = (0, 0, 0, 0)
            else:
                pixels[x, y] = (r, min(g, 232), b, a)
    return image


def components(image):
    width, height = image.size
    alpha = image.getchannel("A")
    alpha_px = alpha.load()
    seen = bytearray(width * height)
    found = []
    for y in range(height):
        for x in range(width):
            index = y * width + x
            if seen[index] or alpha_px[x, y] == 0:
                continue
            queue = deque([(x, y)])
            seen[index] = 1
            coords = []
            while queue:
                cx, cy = queue.popleft()
                coords.append((cx, cy))
                for nx, ny in ((cx + 1, cy), (cx - 1, cy), (cx, cy + 1), (cx, cy - 1)):
                    if nx < 0 or ny < 0 or nx >= width or ny >= height:
                        continue
                    nindex = ny * width + nx
                    if seen[nindex] or alpha_px[nx, ny] == 0:
                        continue
                    seen[nindex] = 1
                    queue.append((nx, ny))
            found.append(coords)
    return found


def keep_main_components(image, threshold):
    found = components(image)
    if not found:
        return image, []
    largest = max(len(component) for component in found)
    keep_min = max(80, int(largest * threshold))
    kept = [component for component in found if len(component) >= keep_min]
    output = Image.new("RGBA", image.size, (0, 0, 0, 0))
    source_px = image.load()
    output_px = output.load()
    for component in kept:
        for x, y in component:
            output_px[x, y] = source_px[x, y]
    return output, [len(component) for component in found]


def edge_touches(bbox, size, margin):
    if not bbox:
        return []
    width, height = size
    left, top, right, bottom = bbox
    touches = []
    if left <= margin:
        touches.append("left")
    if top <= margin:
        touches.append("top")
    if width - right <= margin:
        touches.append("right")
    if height - bottom <= margin:
        touches.append("bottom")
    return touches


def cells_from_grid(args, source):
    slots = [slot.strip() for slot in args.slots.split(",") if slot.strip()]
    if len(slots) != args.cols * args.rows:
        raise SystemExit(f"slot count {len(slots)} does not match grid {args.cols}x{args.rows}")
    source_width, source_height = source.size
    cells = []
    failures = []

    for index, slot in enumerate(slots):
        col = index % args.cols
        row = index // args.cols
        left = round(col * source_width / args.cols)
        right = round((col + 1) * source_width / args.cols)
        top = round(row * source_height / args.rows)
        bottom = round((row + 1) * source_height / args.rows)
        raw_cell = source.crop((left, top, right, bottom))
        transparent, component_sizes = keep_main_components(remove_chroma(raw_cell, args.chroma_key), args.component_threshold)
        bbox = transparent.getbbox()
        if not bbox:
            failures.append(f"{slot}: empty after chroma removal")
            continue
        touches = edge_touches(bbox, transparent.size, args.edge_margin)
        if touches:
            failures.append(f"{slot}: subject touches source cell edge(s): {', '.join(touches)}")
        cropped = transparent.crop(bbox)
        cropped = clean_pose_crop(cropped, args.cleanup_threshold)
        cells.append(
            {
                "slot": slot,
                "source_cell": [left, top, right, bottom],
                "source_bbox": list(bbox),
                "source_size": list(cropped.size),
                "component_sizes": component_sizes,
                "image": cropped,
            }
        )
    return cells, failures


def cells_from_components(args, source):
    slots = [slot.strip() for slot in args.slots.split(",") if slot.strip()]
    transparent = remove_chroma(source, args.chroma_key)
    found = components(transparent)
    if not found:
        return [], ["no components found after chroma removal"]

    largest = max(len(component) for component in found)
    keep_min = max(80, int(largest * args.component_threshold))
    kept = [component for component in found if len(component) >= keep_min]
    source_px = transparent.load()
    source_width, source_height = transparent.size
    raw_components = []
    for component in kept:
        left = min(x for x, _ in component)
        top = min(y for _, y in component)
        right = max(x for x, _ in component) + 1
        bottom = max(y for _, y in component) + 1
        raw_components.append(
            {
                "component_bbox": [left, top, right, bottom],
                "area": len(component),
                "center_x": (left + right) / 2,
                "center_y": (top + bottom) / 2,
            }
        )

    if len(raw_components) < len(slots):
        return [], [f"component count {len(raw_components)} is less than slot count {len(slots)}"]

    try:
        row_groups = split_by_largest_gaps(raw_components, args.rows, "center_y")
        row_groups.sort(key=lambda group: sum(item["center_y"] for item in group) / len(group))
        pose_groups = []
        for row_group in row_groups:
            col_groups = split_by_largest_gaps(row_group, args.cols, "center_x")
            col_groups.sort(key=lambda group: sum(item["center_x"] for item in group) / len(group))
            pose_groups.extend(col_groups)
    except ValueError as exc:
        return [], [str(exc)]

    if len(pose_groups) != len(slots):
        return [], [f"pose group count {len(pose_groups)} does not match slot count {len(slots)}"]

    cells = []
    failures = []
    for slot, group in zip(slots, pose_groups):
        left = min(item["component_bbox"][0] for item in group)
        top = min(item["component_bbox"][1] for item in group)
        right = max(item["component_bbox"][2] for item in group)
        bottom = max(item["component_bbox"][3] for item in group)
        bbox = [left, top, right, bottom]
        touches = edge_touches(tuple(bbox), (source_width, source_height), args.edge_margin)
        if touches:
            failures.append(f"{slot}: subject touches source sheet edge(s): {', '.join(touches)}")
        cropped = transparent.crop((left, top, right, bottom))
        cropped = clean_pose_crop(cropped, args.cleanup_threshold)
        crop_bbox = cropped.getbbox()
        if crop_bbox:
            cropped = cropped.crop(crop_bbox)
        cells.append(
            {
                "slot": slot,
                "source_cell": bbox,
                "source_bbox": [0, 0, cropped.size[0], cropped.size[1]],
                "source_size": list(cropped.size),
                "component_sizes": [item["area"] for item in group],
                "image": cropped,
            }
        )
    return cells, failures


def clean_pose_crop(image, threshold):
    cleaned, _ = keep_main_components(image, threshold)
    return cleaned


def split_by_largest_gaps(items, group_count, key):
    if group_count == 1:
        return [list(items)]
    if len(items) < group_count:
        raise ValueError(f"cannot split {len(items)} items into {group_count} groups")
    ordered = sorted(items, key=lambda item: item[key])
    gaps = []
    for index in range(len(ordered) - 1):
        gaps.append((ordered[index + 1][key] - ordered[index][key], index))
    split_indices = sorted(index for _, index in sorted(gaps, reverse=True)[: group_count - 1])
    groups = []
    start = 0
    for split_index in split_indices:
        groups.append(ordered[start : split_index + 1])
        start = split_index + 1
    groups.append(ordered[start:])
    if any(not group for group in groups):
        raise ValueError("component clustering produced an empty pose group")
    return groups


def main():
    args = parse_args()
    slots = [slot.strip() for slot in args.slots.split(",") if slot.strip()]
    if len(slots) != args.cols * args.rows:
        raise SystemExit(f"slot count {len(slots)} does not match grid {args.cols}x{args.rows}")

    source = Image.open(args.source).convert("RGBA")
    if args.mode == "grid":
        cells, failures = cells_from_grid(args, source)
    else:
        cells, failures = cells_from_components(args, source)

    if failures:
        for failure in failures:
            print(f"ERROR: {failure}")
        raise SystemExit(2)

    target_width, target_height = args.target_size
    reference = next((cell for cell in cells if cell["slot"] == args.reference_slot), cells[0])
    ref_width, ref_height = reference["source_size"]
    max_source_width = max(cell["source_size"][0] for cell in cells)
    max_source_height = max(cell["source_size"][1] for cell in cells)
    shared_scale = min(
        args.target_reference_height / ref_height,
        (target_width - args.side_margin * 2) / max_source_width,
        (args.baseline - args.top_margin) / max_source_height,
    )
    if shared_scale <= 0:
        raise SystemExit("computed non-positive shared scale")

    out_dir = Path(args.out)
    out_dir.mkdir(parents=True, exist_ok=True)
    project_dir = Path(args.project_out) if args.project_out else None
    if project_dir:
        project_dir.mkdir(parents=True, exist_ok=True)

    preview = Image.new("RGBA", (len(cells) * target_width, target_height), (24, 27, 28, 255))
    draw = ImageDraw.Draw(preview)
    manifest = {
        "source": str(Path(args.source)),
        "target_size": [target_width, target_height],
        "baseline": args.baseline,
        "reference_slot": reference["slot"],
        "target_reference_height": args.target_reference_height,
        "shared_scale": shared_scale,
        "frames": [],
    }

    for index, cell in enumerate(cells):
        slot = cell["slot"]
        source_image = cell["image"]
        out_width = max(1, round(source_image.size[0] * shared_scale))
        out_height = max(1, round(source_image.size[1] * shared_scale))
        resized = source_image.resize((out_width, out_height), Image.Resampling.LANCZOS)
        frame = Image.new("RGBA", (target_width, target_height), (0, 0, 0, 0))
        paste_x = round((target_width - out_width) / 2)
        paste_y = args.baseline - out_height
        if paste_x < 0 or paste_y < 0 or paste_x + out_width > target_width or paste_y + out_height > target_height:
            raise SystemExit(f"{slot}: normalized frame would be cropped")
        frame.alpha_composite(resized, (paste_x, paste_y))
        out_path = out_dir / f"{slot}.png"
        frame.save(out_path)
        if project_dir:
            frame.save(project_dir / f"{slot}.png")
        preview.alpha_composite(frame, (index * target_width, 0))
        draw.text((index * target_width + 8, 6), slot, fill=(225, 230, 220, 255))
        out_bbox = frame.getchannel("A").getbbox()
        manifest["frames"].append(
            {
                "slot": slot,
                "source_cell": cell["source_cell"],
                "source_bbox": cell["source_bbox"],
                "source_size": cell["source_size"],
                "output_bbox": list(out_bbox) if out_bbox else None,
                "output_size": [out_width, out_height],
                "paste": [paste_x, paste_y],
            }
        )

    if args.preview:
        Path(args.preview).parent.mkdir(parents=True, exist_ok=True)
        preview.save(args.preview)
    if args.manifest:
        Path(args.manifest).parent.mkdir(parents=True, exist_ok=True)
        Path(args.manifest).write_text(json.dumps(manifest, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(json.dumps({"frames": len(cells), "shared_scale": shared_scale}, ensure_ascii=False))


if __name__ == "__main__":
    main()
