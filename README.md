<h2 align="center">
  <a href="" target="blank_">
    <img src="./doc/image/logo.svg" alt="Logo" height="75">
  </a>
  <br>
  Implementation of Marlowe On Cardano (Runtime)
</h2>
  <p align="center">
    <a href="https://github.com/input-output-hk/marlowe-cardano/releases"><img src="https://img.shields.io/github/v/release/input-output-hk/marlowe-cardano?style=for-the-badge" /></a>
  </p>
<div align="center">
  <a href=""><img src="https://img.shields.io/badge/stability-beta-33bbff.svg" alt="Beta"></a>
  <a href="./LICENSE"><img src="https://img.shields.io/badge/License-Apache_2.0-blue.svg"></a>
  <a href="https://discord.com/invite/cmveaxuzBn"><img src="https://img.shields.io/discord/826816523368005654?label=Chat%20on%20Discord"></a>
</div>

> [!IMPORTANT]
> This Marlowe repository will soon be moved to https://github.com/marlowe-lang. The new repositories will be administered by an independent vehicle, a not-for-profit organization currently being set up by the transition team.<br>
> This will allow us to ensure community representation and stewardship. Future developments and support for Marlowe are transitioning to a community-driven model initially led by [Simon Thompson](https://github.com/simonjohnthompson), [Nicolas Henin](https://github.com/nhenin) and [Tomasz Rybarczyk](https://github.com/paluh). <br>
> See [here](https://github.com/marlowe-lang/.github/blob/main/profile/transition.md) for details.


marlowe-cardano is an implementation of Marlowe for the Cardano blockchain, built on top of Plutus.

This repository contains:

* The Marlowe Runtime and Marlowe Runtime Web
* The implementation of the Marlowe domain-specific language in Haskell.
* Tools for working with Marlowe, including static analysis.
* A selection of examples using Marlowe, including a number based on the ACTUS financial standard.

## Documentation

### User documentation

The main documentation for the whole Plutus ecosystem is located https://plutus.readthedocs.io/en/latest/[here].

An index of key documentation for Marlowe development is located at https://developers.cardano.org/docs/smart-contracts/marlowe/#resources-for-developing-and-deploying-marlowe-contracts.

## Working with the project

### How to submit an issue

Issues can be filed in the https://github.com/input-output-hk/marlowe-cardano/issues[GitHub Issue tracker].

However, note that this is pre-release software, so we will not usually be providing support.

### How to develop and contribute to the project

See link:CONTRIBUTING{outfilesuffix}[CONTRIBUTING], which describes our processes in more detail including development environments; and link:ARCHITECTURE{outfilesuffix}[ARCHITECTURE], which describes the structure of the repository.

### How to depend on the project from another Haskell project

None of our libraries are on Hackage, unfortunately (many of our dependencies aren't either).
So for the time being, you need to:

- Add `marlowe` as a `source-repository-package` to your `cabal.project`.
- Copy the `source-repository-package` stanzas from our `cabal.project` to yours.
- Copy additional stanzas from our `cabal.project` as you need, e.g. you may need some of the `allow-newer` stanzas.

### How to build the project's artifacts

This section contains information about how to build the project's artifacts for independent usage.

#### Prerequisites

The Haskell libraries in the Marlowe project are built with `cabal` and Nix.
The other artifacts (docs etc.) are also most easily built with Nix.

##### Nix

Install https://nixos.org/nix/[Nix] (recommended). following the instructions on the https://nixos.org/nix/[Nix website].

##### Non-Nix

You can build some of the Haskell packages without Nix, but this is not recommended and we don't guarantee that these prerequisites are sufficient.
If you use Nix, these tools are provided for you via `nix develop`, and you do *not* need to install them yourself.

* If you want to build our Haskell packages with https://www.haskell.org/cabal/[`cabal`], then install it.
* If you want to build our Haskell packages with https://haskellstack.org/[`stack`], then install it.
* If you want to build our Agda code, then install https://github.com/agda/agda[Agda] and the https://github.com/agda/agda-stdlib[standard library].


#### How to build the Haskell packages and other artifacts with Nix

Run `nix build .#marlowe-runtime` from the root to build the Marlowe library.

##### How to build the Haskell packages with `cabal`

The Haskell packages can be built directly with `cabal`.
We do this during development.
The best way is to do this is inside a `nix develop`.

>[!NOTE]
> For fresh development setups, you also need to run `cabal update`.


Run `cabal build marlowe` from the root to build the Marlowe library.

See the link:./cabal.project[cabal project file] to see the other packages that you can build with `cabal`.

>[!NOTE]
> If you get errors about missing shared libraries, try running `cabal clean` first. <br>
> If that fails you might have a corrupt cabal store, in which case you should `rm -rf ~/.cabal/store` and try `cabal build all` again.

This repository uses nix to provide the development and build environment.

For instructions on how to install and configure nix (including how to enable access to our binary caches), refer to link:https://github.com/input-output-hk/iogx/blob/main/doc/nix-setup-guide.md.

If you already have nix installed and configured, you may enter the development shell by running `nix develop`.

### Which attributes to use to build different artifacts

Run `list-flake-outputs` while inside the `nix develop` shell for a list of all the artifacts you can build from this repository

## Docker compose

There is a `docker compose` setup designed to give a local developer mode of the marlowe runtime components,
configured in link:./nix/marlowe-cardano/compose.nix[`compose.nix`].

Currently, this only supports Linux systems.

On Linux, `compose.yaml` will be automatically set up for the user when entering `nix develop`.

Running `nix run .#re-up` will refresh `compose.yaml` if need be and then restart any services which have changed.

Services currently included:

* `marlowe-chain-sync`: `marlowe-chain-sync` for the `preprod` network.
* `marlowe-chain-indexer`: `marlowe-chain-indexer` for the `preprod` network.
* `node`: A node for the `preprod` network.
* `postgres`: A postgres instance, for marlowe-chain-sync state.
* `marlowe--sync`: `marlowe-sync` for the `preprod` network.
* `marlowe--indexer`: `marlowe-indexer` for the `preprod` network.
* `marlowe-tx`: A `marlowe-tx` instance.
* `marlowe-contract`: A `marlowe-contract` instance.
* `marlowe-proxy`: A `marlowe-proxy` instance.
* `web`: A `marlowe-web-server` instance.
* `otel-collector`: A shared `opentelemetry` collector instance for distributed tracing.
* `jaeger`: A trace viewer service.

The following commands may be useful:

* `docker compose exec postgres /exec/run-sqitch`: Run the sqitch migrations for the chain-sync database.
* `docker compose exec postgres psql -U postgres -d chain`: Run psql in the `chain` database.
* `docker compose port`, e.g. `docker compose port web 8080` will show the local port that maps to port `8080` for the `web` service

### Accessing the node socket:

The node socket file lives inside a Docker volume. Because it is created by the
container, it is owned by root, and needs elevated permissions (via `sudo`) to
use - keep this in mind when using it locally with a tool like `cardano-cli`.

To list your Docker volumes, use the command `docker volume ls`. The socket
lives in the `marlowe-cardano_shared` volume. Use
`docker volume inspect marlowe-cardano_shared` to obtain information about the
volume. The `Mountpoint` property shows the directory on the host machine that
maps to the volume (one-liner: `docker volume inspect marlowe-cardano_shared | jq -r '.[].Mountpoint'`)

To use this with `cardano-cli`:

```bash
export CARDANO_NODE_SOCKET_PATH=$(docker volume inspect marlowe-cardano_shared | jq -r '.[].Mountpoint')
# -E passes the current environment to sudo
sudo -E cardano-cli ...
```


