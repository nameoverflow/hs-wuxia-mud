#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  storyboard-from-video.sh <video> <out-dir> [segment_seconds=3] [fps=16] [cols=8] [tile_width=240]

Creates dense storyboard/contact-sheet PNGs from a local video. Each output image
contains one time segment tiled left-to-right, top-to-bottom.
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

video="${1:-}"
out_dir="${2:-}"
segment_seconds="${3:-3}"
fps="${4:-16}"
cols="${5:-8}"
tile_width="${6:-240}"

if [[ -z "$video" || -z "$out_dir" ]]; then
  usage >&2
  exit 2
fi

if [[ ! -f "$video" ]]; then
  echo "Video not found: $video" >&2
  exit 1
fi

if ! command -v ffmpeg >/dev/null 2>&1 || ! command -v ffprobe >/dev/null 2>&1; then
  echo "ffmpeg and ffprobe are required." >&2
  exit 1
fi

mkdir -p "$out_dir"

duration="$(
  ffprobe -v error -show_entries format=duration -of default=nw=1:nk=1 "$video" |
    awk '{ if ($1 > 0) printf "%.3f", $1; else print "0" }'
)"

if [[ "$duration" == "0" || "$duration" == "0.000" ]]; then
  echo "Could not determine video duration: $video" >&2
  exit 1
fi

frames_per_segment="$(awk -v s="$segment_seconds" -v f="$fps" 'BEGIN { n = int(s * f + 0.999); if (n < 1) n = 1; print n }')"
rows="$(awk -v n="$frames_per_segment" -v c="$cols" 'BEGIN { print int((n + c - 1) / c) }')"
count="$(awk -v d="$duration" -v s="$segment_seconds" 'BEGIN { print int((d + s - 0.000001) / s) }')"

printf "video=%s\nout_dir=%s\nduration=%ss\nsegment_seconds=%s\nfps=%s\ntile=%sx%s\n\n" \
  "$video" "$out_dir" "$duration" "$segment_seconds" "$fps" "$cols" "$rows"

for ((i = 0; i < count; i++)); do
  start="$(awk -v i="$i" -v s="$segment_seconds" 'BEGIN { printf "%.3f", i * s }')"
  idx="$(printf "%03d" "$((i + 1))")"
  out="$out_dir/storyboard-$idx.png"

  ffmpeg -hide_banner -loglevel error -y \
    -ss "$start" -t "$segment_seconds" -i "$video" \
    -vf "fps=${fps},scale=${tile_width}:-1:flags=lanczos,tile=${cols}x${rows}:margin=10:padding=4:color=0x111111" \
    -frames:v 1 "$out"

  echo "$out"
done
