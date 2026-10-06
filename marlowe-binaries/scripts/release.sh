#!/bin/sh
# Release a CIP-0057 blueprint for the Marlowe validators.
#
# Pipeline:
#   1. Compile semantics + payout validators (Production variant, the on-chain bytes).
#   2. Read the first 8 hex chars of each script hash.
#   3. Write the blueprint to a temp path with the compiled script bytes embedded.
#   4. Read `preamble.version` from the generated JSON — the cabal file's
#      `version:` field is the single source of truth, propagated via
#      Paths_marlowe_plutus.version; we read it back from the artifact.
#   5. Move the temp file to blueprints/<version>-<8hex>-<8hex>.plutus.json
#      and write a SHA-256 sidecar.
#   6. Self-validate via scripts/check-blueprint.sh.
#
# Run from the repo root inside `nix develop`:
#
#   ./marlowe-binaries/scripts/release.sh
#
# Override the output directory with:
#
#   ./marlowe-binaries/scripts/release.sh /path/to/output-dir

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
scripts_dir="$repo_root/marlowe-binaries/scripts"
output_dir="${1:-$repo_root/marlowe-binaries/blueprints}"
mkdir -p "$output_dir"

build_dir="$(mktemp -d -t marlowe-blueprint-release.XXXXXX)"
trap 'rm -rf "$build_dir"' EXIT

echo "== Compiling semantics validator (Production) =="
cabal run marlowe-binaries -- compile marlowe --output-dir "$build_dir" --message-format text

echo "== Compiling payout validator (Production) =="
cabal run marlowe-binaries -- compile payout --output-dir "$build_dir" --message-format text

semantics_hash="$(tr -d '[:space:]' < "$build_dir/marlowe-semantics.plutus.hash")"
payout_hash="$(tr -d '[:space:]' < "$build_dir/marlowe-rolepayout.plutus.hash")"
semantics_short="$(printf '%s' "$semantics_hash" | cut -c1-8)"
payout_short="$(printf '%s' "$payout_hash" | cut -c1-8)"

temp_output="$build_dir/blueprint.plutus.json"
echo "== Writing blueprint to temp path =="
cabal run marlowe-binaries -- blueprint "$temp_output"

# Extract the version from the blueprint's preamble. The cabal file's
# `version:` is the single source of truth; Paths_marlowe_plutus propagates
# it into preambleVersion at build time, and we read it back here so the
# shell script never has to duplicate the string.
release_version="$(grep -o '"version"[[:space:]]*:[[:space:]]*"[^"]*"' "$temp_output" | head -n1 | sed -E 's/.*"([^"]*)"$/\1/')"
if [ -z "$release_version" ]; then
  echo "ERROR: could not parse preamble.version from generated blueprint" >&2
  exit 1
fi

output_file="$output_dir/${release_version}-${semantics_short}-${payout_short}.plutus.json"
mv "$temp_output" "$output_file"

echo "== Blueprint version: $release_version =="
echo "== Moved to: $output_file =="

echo "== Computing SHA-256 sidecar =="
( cd "$(dirname "$output_file")" && sha256sum "$(basename "$output_file")" > "$(basename "$output_file").sha256" )
cat "$(dirname "$output_file")/$(basename "$output_file").sha256"

echo "== Self-validating =="
"$scripts_dir/check-blueprint.sh" "$output_file"

echo
echo "OK: $output_file"
echo "    semantics script hash: $semantics_hash"
echo "    payout    script hash: $payout_hash"
