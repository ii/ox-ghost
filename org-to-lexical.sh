#!/bin/bash
# Stage 2: Export rendered org file to Lexical JSON
# Usage: ./org-to-lexical.sh input.org [output.json]
#
# This script converts a rendered org file to Ghost Lexical JSON.
# It does NOT execute babel blocks - use render-template.sh for Stage 1.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OX_LEXICAL="$SCRIPT_DIR/ox-lexical.el"

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 input.org [output.json]" >&2
    echo "" >&2
    echo "Stage 2 of the org-to-Ghost pipeline." >&2
    echo "Converts rendered org to Lexical JSON (no babel execution)." >&2
    exit 1
fi

INPUT="$1"
OUTPUT="${2:-}"

if [[ ! -f "$INPUT" ]]; then
    echo "Error: Input file not found: $INPUT" >&2
    exit 1
fi

if [[ ! -f "$OX_LEXICAL" ]]; then
    echo "Error: ox-lexical.el not found at $OX_LEXICAL" >&2
    exit 1
fi

# Export to Lexical JSON
# - org-export-use-babel nil: Don't try to execute babel blocks
# - This is Stage 2: we expect results already baked into the file
EMACS_CMD=(
    emacs --batch -Q
    --eval "(require 'org)"
    --eval "(require 'ox-html)"
    --eval "(setq org-export-use-babel nil)"
    -l "$OX_LEXICAL"
    --visit "$INPUT"
    --eval "(princ (org-export-as 'lexical))"
)

if [[ -n "$OUTPUT" ]]; then
    "${EMACS_CMD[@]}" > "$OUTPUT"
    echo "Exported: $OUTPUT"
else
    "${EMACS_CMD[@]}"
fi
