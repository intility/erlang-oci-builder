#!/usr/bin/env bash
# Validates an SPDX SBOM.
#
#   1. spdx-tools: the document must be valid SPDX (blocking)
#   2. NTIA minimum elements: report only for now, becomes blocking once the SBOM
#      carries suppliers and every package is attached to the dependency tree
#
# Usage: scripts/validate_sbom.sh <sbom.spdx.json>
set -euo pipefail

SBOM="${1:?usage: validate_sbom.sh <sbom.spdx.json>}"

VENV="$(mktemp -d)"
trap 'rm -rf "$VENV"' EXIT
python3 -m venv "$VENV"
"$VENV/bin/pip" install --quiet spdx-tools==0.8.5 ntia-conformance-checker==6.0.0

echo "== SPDX validation"
"$VENV/bin/pyspdxtools" -i "$SBOM"
echo "valid"

echo "== NTIA minimum elements (report only)"
"$VENV/bin/sbomcheck" -c ntia "$SBOM" || echo "::warning::SBOM does not meet the NTIA minimum elements yet"
