# `marlowe-contrib`

An umbrella package grouping small, dependency-light helper libraries that are
shared across the Marlowe project but do not belong to any single application
or core library. Each helper lives in its own sublibrary so consumers can pull
in only what they actually depend on.

Current sub-libraries:

- `marlowe-contrib:optparse` — shared `optparse-applicative` definitions
  (notably the `--message-format` flag and `emit*` helpers used by every Marlowe
  CLI tool).
- `marlowe-contrib:foldable-traversable` — small helpers built on top of
  `Data.Foldable` (flipped variants of `foldMap`/`foldMapM`/`foldlM`/`foldrM`,
  `tillFirstMatch`, `ifoldMapM`), exposed as `Marlowe.Contrib.Foldable`.