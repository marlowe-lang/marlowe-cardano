#!/usr/bin/env python3
"""Verify every `$ref` in a CIP-0057 blueprint resolves to a key in the
in-document `definitions` map, and list any unused definitions for context.

The CIP-0057 meta-schema does NOT enforce this: its `definitions` is
typed as `additionalProperties: true`. We confirmed (see git history)
that a blueprint with a `$ref` pointing to a non-existent key passes
`check-jsonschema` validation with exit code 0.

This is a known limitation of `check-jsonschema` upstream, not
something specific to our setup. Related upstream issues:
  - python-jsonschema/check-jsonschema#464 — "--check-metaschema doesn't
    check $ref referenced schemas" (Jul 2024, still open)
  - python-jsonschema/check-jsonschema#640 — "Unable to resolve nested
    `$ref`s when using local files" (Jan 2026, still open)
  - python-jsonschema/check-jsonschema#700 — "Fix nested local reference
    resolution" (PR open)

This script is the belt-and-suspenders. It walks every `$ref` in the blueprint
and verifies a matching key exists in `definitions`. Additionally, it identifies any definitions that are not referenced by any `$ref`.
Exits 0 if every `$ref` resolves and there are no unused definitions, otherwise exits 1.

Usage:
    check-blueprint-refs.py path/to/plutus.json
"""

import json
import sys


def main(path):
    with open(path) as f:
        doc = json.load(f)
    defs = set(doc.get("definitions", {}).keys())
    refs = set()

    def walk(node):
        if isinstance(node, dict):
            for k, v in node.items():
                if k == "$ref" and isinstance(v, str) and v.startswith("#/definitions/"):
                    refs.add(v[len("#/definitions/"):])
                walk(v)
        elif isinstance(node, list):
            for item in node:
                walk(item)

    walk(doc)
    missing = refs - defs
    unused = defs - refs

    print(f"refs: {len(refs)}  defs: {len(defs)}")
    if missing:
        print(f"missing refs: {sorted(missing)}")
    if unused:
        print(f"unused defs: {sorted(unused)}")

    sys.exit(1 if (missing or unused) else 0)


if __name__ == "__main__":
    main(sys.argv[1])
