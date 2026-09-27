"""Mechanical processing of the accepted image_gen originals. Requires Pillow."""
from pathlib import Path
import hashlib
import json
from PIL import Image

job = Path(__file__).resolve().parent
repo = job.parents[3]
out = repo / "client/src/assets/battle/ink-stage-v1"
out.mkdir(parents=True, exist_ok=True)
background = job / "generated/backdrop-source.png"
effects = job / "generated/vfx-source.png"
Image.open(background).convert("RGB").resize((1280, 640), Image.Resampling.LANCZOS).save(out / "backdrop.webp", quality=88)
sheet = Image.open(effects).convert("RGBA")
assert sheet.size == (1536, 1024), "Source atlas layout must remain 3 x 2 cells of 512px"
for index, name in enumerate(["thrust", "slash", "rising", "impact", "parry", "aura"]):
    x, y = index % 3 * 512, index // 3 * 512
    cell = sheet.crop((x, y, x + 512, y + 512))
    cell.thumbnail((232, 232), Image.Resampling.LANCZOS)
    padded = Image.new("RGBA", (256, 256))
    padded.alpha_composite(cell, (12, 12))
    padded.save(out / f"{name}.webp", lossless=True)
manifest = {
    "tool": "built-in image_gen",
    "sources": {"background": str(background.relative_to(repo)), "vfx": str(effects.relative_to(repo))},
    "processing": "resize, 3x2 atlas slicing, 12px transparent padding, WebP encoding; no artwork redrawn",
    "assets": [{"path": str(p.relative_to(repo)), "bytes": p.stat().st_size, "sha256": hashlib.sha256(p.read_bytes()).hexdigest()} for p in sorted(out.glob("*.webp"))]
}
(job / "assets.json").write_text(json.dumps(manifest, indent=2) + "\n")
print("Prepared stage artwork. Next: cd client && npm run pack:battle")
