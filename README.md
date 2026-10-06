<h2 align="center">
  <a href="" target="blank_">
    <img src="./doc/image/logo.svg" alt="Logo" height="75">
  </a>
  <br>
  Implementation of Marlowe On Cardano. On-chain validators, the runtime, and the tools around them.
</h2>
  <p align="center">
    <a href="https://github.com/input-output-hk/marlowe-cardano/releases"><img src="https://img.shields.io/github/v/release/input-output-hk/marlowe-cardano?style=for-the-badge" /></a>
  </p>
<div align="center">
  <a href=""><img src="https://img.shields.io/badge/stability-beta-33bbff.svg" alt="Beta"></a>
  <a href="./LICENSE"><img src="https://img.shields.io/badge/License-Apache_2.0-blue.svg"></a>
</div>

Marlowe-Cardano is an implementation of Marlowe for the Cardano blockchain, built on top of Plutus.

This repository contains:

* The implementation of the Marlowe domain-specific language in Plutus.
* The Marlowe Runtime which provides a REST API for interacting with Marlowe contracts on the Cardano blockchain.
* Tools for working with Marlowe, including static analysis.

## Documentation

Please head up to http://marlowe-lang.org/ for the latest documentation.

## Development

Nix setup, including the binary caches: <https://github.com/input-output-hk/iogx/blob/main/doc/nix-setup-guide.md>

```shell
$ nix develop
$ cabal build all
```

## History

On 6 October 2026 the files on main were replaced with a tree reconstructed from scratch. Those new commits are the first parent of the join. git log --first-parent follows them. git blame on a rewritten line names that reconstruction, because the line was written again.

The earlier commits were not removed. They are the second parent of the join, and the old files are the tag legacy-final:

git checkout legacy-final
git shortlog -sn legacy-final

## License

Apache-2.0. See LICENSE and NOTICE.


<!--
# Marlowe Plutus Validators

This project implements the on-chain component of the Cardano implementation of Marlowe as a Plutus smart contract.
The main outputs are the marlowe semantics validator, which checks the spending
of Marlowe script outputs, and the marlowe role payout validator, which checks
the spending of role payouts.

For testing support, fixture generation, and reference data, see `marlowe-plutus/marlowe-testing/README.md`.


## Repo Structure

The Haskell/Plinth code is structured as follows:

```shell
.
├── libs                # Shared internal libraries, adaptors, and utilities
├── marlowe-binaries    # Separated plutus-tx compilation pipeline and scripts generation
└── marlowe-plutus      # Marlowe implementation in Plinth together with test suite
```

### marlowe-plutus

#### marlowe-testing

`marlowe-testing` contains shared testing utilities, reference fixtures, and the fixture-generation executable.
See `marlowe-plutus/marlowe-testing/README.md` for details.

### marlowe-binaries

`marlowe-binaries` provides the CLI for compiling scripts and working with benchmark fixtures. For example, `cabal run marlowe-binaries -- compile --message-format json | cabal run marlowe-binaries -- benchmark generate --output-dir benchmarks` compiles the default production scripts, prints a JSON `CompileResponse`, and pipes it into benchmark generation.
By default, `compile` writes scripts to `out/`, `benchmark generate` writes fixtures under `benchmarks/semantics` and `benchmarks/rolepayout`, and `benchmark run` reads from the packaged `benchmarks/` directory unless `--benchmark-dir` is provided.




## Dev Shell

This repository uses nix to provide the development and build environment.

For instructions on how to install and configure nix (including how to enable access to our binary caches), refer to [this document](https://github.com/input-output-hk/iogx/blob/main/doc/nix-setup-guide.md). 

If you already have nix installed and configured, you may enter the development shell by running `nix develop`.

If you have direnv installed, you can have the shell automatically load and 
refresh for you by running these commands:

```bash
mkdir .direnv
direnv allow
```

Now, whenever you enter the repo the shell will be automatically loaded for you
and will be refreshed when the environment changes.

Once in the dev shell, type `info` to see the available commands and environment.

## Compiling the project

From the dev shell, you can compile the project with `cabal build all`.

Alternatively, you can compile with `nix` using `nix build .#marlowe-validators`

## Compiling the validators

You can compile the validators with the CLI:

```bash
cabal run marlowe-binaries -- compile
```

This writes the default production scripts to `out/`:

- `out/marlowe-rolepayout.plutus` - the role payout validator as a JSON-encoded CBOR text-envelope
- `out/marlowe-semantics.plutus` - the Marlowe validator as a JSON-encoded CBOR text-envelope

Use `--devel-scripts` to preserve tracing and `--message-format text|json|yaml` to control command output.
-->

