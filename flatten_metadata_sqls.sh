#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="${1:-MetadataSqls}"

if [[ ! -d "$ROOT_DIR" ]]; then
  echo "ERROR: Folder not found: $ROOT_DIR" >&2
  exit 1
fi

shopt -s nullglob

# Iterate immediate subfolders (servers)
for server_dir in "$ROOT_DIR"/*/; do
  [[ -d "$server_dir" ]] || continue

  server_name="$(basename "${server_dir%/}")"

  # For each .sql in that server folder
  for src in "$server_dir"*.sql; do
    [[ -f "$src" ]] || continue

    base="$(basename "$src")"            # e.g. "1. Tables.sql"
    stem="${base%.sql}"                  # e.g. "1. Tables"

    # remove leading "NN." (with optional spaces): "1. ", "01. ", "3."
    item="$(printf '%s' "$stem" | sed -E 's/^[[:space:]]*[0-9]+[[:space:]]*\.[[:space:]]*//')"

    # normalize item -> safe filename part
    #  - spaces -> underscores
    #  - remove everything not A-Z a-z 0-9 _ -
    item="$(printf '%s' "$item" | tr ' ' '_' | sed -E 's/[^A-Za-z0-9_-]+//g')"

    if [[ -z "$item" ]]; then
      echo "WARN: Could not derive item from: $src (skipping)" >&2
      continue
    fi

    dst="$ROOT_DIR/${server_name}_${item}.sql"
    cp -f "$src" "$dst"
    echo "OK: $dst"
  done
done
