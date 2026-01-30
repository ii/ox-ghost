#!/bin/bash
# ox-ghost-export.sh - Export org file to Lexical JSON
# Usage: ./ox-ghost-export.sh input.org [output.json]
#
# Converts an org file to Ghost Lexical JSON.
# Does NOT execute babel blocks - use render-template.sh for that.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OX_LEXICAL="$SCRIPT_DIR/ox-ghost.el"

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
    echo "Error: ox-ghost.el not found at $OX_LEXICAL" >&2
    exit 1
fi

# Export to Lexical JSON
# - org-export-use-babel nil: Don't try to execute babel blocks
# - org-html-htmlize-output-type nil: Disable htmlize (Ghost uses Prism.js)
# Timeout after 30s to prevent runaway processes
# Also limit recursion depth and disable dangerous features
EMACS_CMD=(
    timeout 30 emacs --batch -Q
    --eval "(require 'org)"
    --eval "(require 'ox-html)"
    --eval "(setq max-lisp-eval-depth 800)"
    --eval "(setq max-specpdl-size 1600)"
    --eval "(setq org-export-use-babel nil)"
    --eval "(setq org-export-with-broken-links t)"
    --eval "(setq org-html-htmlize-output-type nil)"
    --eval "(setq org-confirm-babel-evaluate nil)"
    -l "$OX_LEXICAL"
    --visit "$INPUT"
    --eval "(condition-case err (princ (org-export-as 'ghost)) (error (message \"Export error: %s\" err) (kill-emacs 1)))"
)

if [[ -n "$OUTPUT" ]]; then
    # Capture stderr separately, only show on failure
    ERRFILE=$(mktemp)
    if "${EMACS_CMD[@]}" > "$OUTPUT" 2>"$ERRFILE"; then
        # Validate JSON
        if head -c1 "$OUTPUT" | grep -q '{'; then
            rm -f "$ERRFILE"
            echo "Exported: $OUTPUT"
        else
            echo "Error: Output is not valid JSON" >&2
            cat "$ERRFILE" >&2
            rm -f "$ERRFILE" "$OUTPUT"
            exit 1
        fi
    else
        echo "Error: Emacs export failed" >&2
        cat "$ERRFILE" >&2
        rm -f "$ERRFILE"
        exit 1
    fi
else
    "${EMACS_CMD[@]}" 2>/dev/null
fi
