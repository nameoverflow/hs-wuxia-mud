#!/usr/bin/env bash
set -euo pipefail

out_dir="reports/portrait-qa/$(date +%Y%m%d-%H%M%S)"
expected_size="1254x1254"
cols=3
tile=220
images=()

usage() {
  cat <<'EOF'
Usage:
  qa-portrait-set.sh [--out DIR] [--expected-size WxH|any] [--cols N] [--tile PX] IMAGE...

Validates portrait dimensions with ffprobe, writes manifest.tsv, and creates contact-sheet.png.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --out)
      out_dir="$2"
      shift 2
      ;;
    --expected-size)
      expected_size="$2"
      shift 2
      ;;
    --cols)
      cols="$2"
      shift 2
      ;;
    --tile)
      tile="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      while [[ $# -gt 0 ]]; do
        images+=("$1")
        shift
      done
      ;;
    -*)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
    *)
      images+=("$1")
      shift
      ;;
  esac
done

if [[ ${#images[@]} -eq 0 ]]; then
  echo "No images provided." >&2
  usage >&2
  exit 2
fi

if ! command -v ffprobe >/dev/null 2>&1; then
  echo "ffprobe is required." >&2
  exit 2
fi

if ! command -v ffmpeg >/dev/null 2>&1; then
  echo "ffmpeg is required." >&2
  exit 2
fi

frame_dir="$out_dir/.frames"
rm -rf "$frame_dir"
mkdir -p "$frame_dir"
manifest="$out_dir/manifest.tsv"
: > "$manifest"
printf "index\tdimensions\tstatus\tpath\n" >> "$manifest"

status=0
idx=0
for img in "${images[@]}"; do
  if [[ ! -f "$img" ]]; then
    printf "%03d\tMISSING\tfail\t%s\n" "$idx" "$img" >> "$manifest"
    echo "Missing image: $img" >&2
    status=1
    idx=$((idx + 1))
    continue
  fi

  dim="$(ffprobe -v error -select_streams v:0 -show_entries stream=width,height -of csv=p=0:s=x "$img")"
  item_status="pass"
  if [[ "$expected_size" != "any" && "$dim" != "$expected_size" ]]; then
    item_status="fail"
    status=1
  fi
  printf "%03d\t%s\t%s\t%s\n" "$idx" "$dim" "$item_status" "$img" >> "$manifest"
  cp "$img" "$frame_dir/$(printf '%03d.png' "$idx")"
  idx=$((idx + 1))
done

rows=$(( (idx + cols - 1) / cols ))
if [[ "$idx" -gt 0 ]]; then
  ffmpeg -y -loglevel error \
    -framerate 1 \
    -start_number 0 \
    -i "$frame_dir/%03d.png" \
    -vf "scale=${tile}:${tile}:force_original_aspect_ratio=decrease,pad=${tile}:${tile}:(ow-iw)/2:(oh-ih)/2:color=0xd8bf83,tile=${cols}x${rows}" \
    -frames:v 1 \
    "$out_dir/contact-sheet.png"
fi

rm -rf "$frame_dir"

echo "Manifest: $manifest"
echo "Contact sheet: $out_dir/contact-sheet.png"

exit "$status"
