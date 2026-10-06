#!/bin/sh
# Validate an existing CIP-0057 blueprint.
#
# Two checks:
#   1. Structural validation against the CIP-0057 meta-schema (vendored
#      under ./cip57/). The meta-schema's `definitions` field is typed as
#      `additionalProperties: true`, so this step alone does not enforce
#      that every `$ref` resolves. We saw broken references slip past
#      `check-jsonschema` in practice.
#   2. Belt-and-suspenders `$ref` resolution check (Python). Walks every
#      `$ref` and verifies a matching key exists in `definitions`.
#
# Usage:
#     ./check-blueprint.sh                          # uses plutus.json
#     ./check-blueprint.sh path/to/blueprint.json   # custom path
#
# The validation uses `check-jsonschema` against the vendored
# plutus-blueprint.json. We pass --base-uri so the Python `referencing`
# library resolves nested meta-schemas (plutus-blueprint-argument.json,
# plutus-blueprint-parameter.json, plutus-data.json, plutus-builtin.json)
# from the local cip57/ directory instead of fetching them from
# cips.cardano.org.

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
output_file="${1:-plutus.json}"

if [ ! -f "$output_file" ]; then
  echo "ERROR: $output_file does not exist; check-blueprint.sh only validates, it does not generate." >&2
  exit 1
fi

echo "== Validating $output_file against CIP-0057 meta-schema =="
check-jsonschema \
  --schemafile "$script_dir/cip57/plutus-blueprint.json" \
  --base-uri "file://$script_dir/cip57/" \
  "$output_file"

echo '== Verifying $ref strings against the definitions map =='
python3 "$script_dir/check-blueprint-refs.py" "$output_file"

echo "OK: $output_file passes CIP-0057 validation."
