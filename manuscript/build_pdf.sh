#!/usr/bin/env bash
# Rebuild the robustness-analysis manuscript PDF via pandoc → typst.
# Usage: ./build_pdf.sh   (from manuscript/) or  manuscript/build_pdf.sh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

INPUT="${1:-robustness_analysis_manuscript.md}"
OUTPUT="${2:-robustness_analysis_manuscript.pdf}"
TEMPLATE_TYP="$SCRIPT_DIR/typst-template.typ"
PANDOC_TMPL="$SCRIPT_DIR/pandoc-typst.template"

if ! command -v pandoc >/dev/null 2>&1; then
  echo "error: pandoc not found" >&2
  exit 1
fi
if ! command -v typst >/dev/null 2>&1; then
  echo "error: typst not found (required as --pdf-engine)" >&2
  exit 1
fi
if [[ ! -f "$INPUT" ]]; then
  echo "error: source not found: $INPUT" >&2
  exit 1
fi
if [[ ! -f "$TEMPLATE_TYP" || ! -f "$PANDOC_TMPL" ]]; then
  echo "error: missing typst-template.typ or pandoc-typst.template in $SCRIPT_DIR" >&2
  exit 1
fi

# Pandoc resolves -V template= relative to the source file directory and
# stages it for typst; keep a path relative to this manuscript/ folder.
pandoc "$INPUT" \
  -o "$OUTPUT" \
  --pdf-engine=typst \
  --template="$PANDOC_TMPL" \
  -V "template=typst-template.typ" \
  --from=markdown \
  --wrap=preserve

echo "Wrote $SCRIPT_DIR/$OUTPUT"
