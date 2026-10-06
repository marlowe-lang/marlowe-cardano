# `marlowe-binaries`

This package is a thin wrapper around `marlowe-plutus` that:

1. compiles the Marlowe validator scripts (semantics, role payout, open
   roles, role token minting policy),
2. emits a CIP-0057 contract blueprint (`plutus.json`) for the
   semantics and role-payout validators, and
3. runs benchmark suites against the compiled scripts.

The on-chain code (Plinth sources, validator logic) lives in
`marlowe-plutus`. This package owns the *binary* pipeline: compilation,
script-byte emission, and blueprint generation.

## Rationale

We separated the binary pipeline from the on-chain code to isolate the Plinth compiler invocation:

  * Referencing libraries which contain compilation invocations like `$$(PlutusTx.compile [||...||])` can break the HLS from our experience.

  * Libraries which contain plugin invocations seem to fail in the context of fast dev builds and typechecks (`cabal build "$pkg" --disable-optimization -j ...` or 
    `"printf ':q\\n' | cabal repl $target_desc ${extra[*]} --disable-optimization ...`)

## Building

Given the above rationale, the `marlowe-binaries` package is built with optimizations enabled - please use standard:

```bash
cabal build marlowe-binaries
```

or

```bash
cabal run marlowe-binaries -- <subcommand> [flags]
```

## Releasing a blueprint

`scripts/release.sh` compiles the Production validators, embeds the
script bytes in the blueprint, computes a SHA-256 sidecar, and runs
`scripts/check-blueprint.sh` to self-validate. Run from the repo root
inside `nix develop`:

```bash
./marlowe-binaries/scripts/release.sh
```

The output lands at
`marlowe-binaries/blueprints/<release-version>-<8hex-semantics>-<8hex-payout>.plutus.json`
where the hex tokens are the first 8 chars of each script hash — that
ties the filename to the on-chain identity. The version tag is read from
the `marlowe-plutus.cabal` file (and not `marlowe-binaries.cabal`!).

`scripts/check-blueprint.sh`
is also CI-friendly and only validates; it never regenerates:

```bash
./marlowe-binaries/scripts/check-blueprint.sh path/to/blueprint.json
```

