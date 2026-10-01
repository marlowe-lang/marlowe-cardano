# Marlowe NixOS services

Production units for the indexer, the runtime, Postgres, and a Cardano node.
The indexer and the runtime share a database. They do not depend on each other's processes, so they can run on one machine or on two.

## Services

Long-running units:

| Unit                      | Module                               | Role                                                                                                                  |
|---                        |---                                   |---                                                                                                                    |
| `postgresql.service`      | NixOS                                | The database. Neither Marlowe service owns this unit.                                                                 |
| `cardano-node.service`    | `cardano-node`                       | Local node. Socket defaults to `/run/cardano-node/node.socket`, group `cardano-node`.                                 |
| `marlowe-indexer.service` | [indexer.nix](nix/nixos/indexer.nix) | Follows the chain and writes `marlowe.*`. Restarts until the node and the schema are ready.                           |
| `marlowe-runtime.service` | [runtime.nix](nix/nixos/runtime.nix) | HTTP API on port 8090. Reads the indexer database through `database.uri`. Does not require `marlowe-indexer.service`. |

Helpers. Each is a oneshot and is not a process you operate by hand:

| Unit                              | When it runs                                                                    | What it does                                                                                                                                                                                                                                             |
|---                                |---                                                                              |---                                                                                                                                                                                                                                                       |
| `cardano-node-bootstrap.service`  | Deploy only, [cardano-node-bootstrap.nix](nix/nixos/cardano-node-bootstrap.nix) | If `db/immutable` is missing, restores it with Mithril (`preprod`, `preview`, or `mainnet`), then the node starts. The test VM does not import this module.                                                                                              |
| `cardano-node-ready.service`      | Whenever the indexer is enabled                                                 | `cardano-node` is `Type=simple` and does not notify systemd. This oneshot polls `cardano-cli query tip` (up to 10 minutes) and only then becomes active. It is bound to the node: if the node stops, the gate is dropped and the next start waits again. |
| `marlowe-indexer-migrate.service` | Whenever `database.migrate` is true                                             | Runs Sqitch as the indexer role, then grants the reader roles. The indexer waits for it. The runtime does not.                                                                                                                                           |

`cardano-node-ready` needs `services.marlowe-indexer.cardanoCli`. On a testnet it sets `CARDANO_NODE_NETWORK_ID` and passes `--testnet-magic`. On mainnet it passes `--mainnet` and leaves the variable unset.

The indexer joins `services.marlowe-indexer.socketGroup` (default `cardano-node`) so it can connect to the node socket. The runtime does the same via `services.marlowe-runtime.socketGroup`.

## Database

Postgres is its own service. The `postgres` superuser creates roles and databases. Applications do not connect as `postgres`, and `postgres` does not own the schema.

Two roles:

- `marlowe-indexer` owns the database and the `marlowe` schema. Sqitch runs as this role, which is what allows `CREATE` and `ALTER`. `ensureDBOwnership` is on, so the database must have the same name as the role.
- `marlowe-runtime` is a second login. It does not own a database. `ensureDBOwnership` stays off. The indexer module does not know this name.

Login and table access are different gates.

Peer authentication, in the host config, decides whether a Unix user may open a connection as a role. A successful login still has no rights on `marlowe.block`.

After `sqitch deploy`, `marlowe-indexer-migrate` grants each name in `services.marlowe-indexer.database.readers`:

```sql
GRANT USAGE ON SCHEMA marlowe TO "<role>";
GRANT SELECT ON ALL TABLES IN SCHEMA marlowe TO "<role>";
ALTER DEFAULT PRIVILEGES IN SCHEMA marlowe
  GRANT SELECT ON TABLES TO "<role>";
```

The list is empty when the runtime is not a local role. A later Sqitch change that adds tables is covered by `ALTER DEFAULT PRIVILEGES` only for tables created afterwards by the indexer role. This grant is `SELECT`. If the runtime writes, this is the wrong privilege.

Same machine or another machine is only the host in the URI. A combined guest uses `postgresql://marlowe-runtime@/marlowe-indexer`. A split deploy uses a remote host, a password, and a `host` authentication line. The units stay the same. If the runtime starts before migrations have finished, it fails and restarts. That is the intended failure.

## Testing

### What the VM is

`mkDeploymentTest` in [test-deployment.nix](nix/nixos/test-deployment.nix) returns a `nixosSystem`. It is not a service module. [flake-outputs.nix](nix/nixos/flake-outputs.nix) exports it next to the modules:

```nix
nixosConfigurations.deployment-test = nixos.mkDeploymentTest {
  cardanoNodeModule = inputs.cardano-node.nixosModules.cardano-node;
  cardanoCli = inputs.cardano-node.packages.x86_64-linux.cardano-cli;
  indexerPackage = inputs.self.packages.x86_64-linux.marlowe-indexer;
  hostSnapshot =
    let path = builtins.getEnv "MARLOWE_PREPROD_DB";
    in if path == ""
       then throw "MARLOWE_PREPROD_DB is not set"
       else path;
};
```

The guest imports the upstream `cardano-node` module and [indexer.nix](nix/nixos/indexer.nix), then enables Postgres, the node, and the indexer. The node database is the host snapshot, mounted with 9p at `/mnt/preprod-db`. Mithril does not run inside this VM. The guest disk is a `nixos.qcow2` created in the directory where you start the VM. Postgres lives on that disk. The snapshot does not.

`builtins.getEnv` is empty in a pure evaluation, so the build is impure on purpose. The path is absolute because 9p resolves it on the host when QEMU starts, not relative to the flake.

### Running it

The snapshot is a Mithril `db` directory: the one that contains `immutable/`. Download it once on the host. `mithril-client` is `inputs.mithril.packages.${system}.mithril-client-cli`.

```bash
export AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
export GENESIS_VERIFICATION_KEY="$(curl -fsSL https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey)"
export ANCILLARY_VERIFICATION_KEY="$(curl -fsSL https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/release-preprod/ancillary.vkey)"
export MITHRILL_SNAPSHOT_DIR="$(pwd)/preprod-db"

mkdir -p /var/lib/cardano/preprod
mithril-client cardano-db download \
  --include-ancillary \
  --download-dir $MITHRILL_SNAPSHOT_DIR \
  latest
```

Point `MARLOWE_PREPROD_DB` at that `$MITHRILL_SNAPSHOT_DIR/db` directory.

9p passes the guest uid through. The node service is not your host uid, so creating `lock` fails unless the directory is writable by the guest:

```bash
chmod -R a+rwX $MITHRILL_SNAPSHOT_DIR
```

Build and start on the serial console. Clicking a QEMU window is not required, and the console can be copied:

```bash
sudo env MARLOWE_PREPROD_DB=$MITHRILL_SNAPSHOT_DIR/db \
  nixos-rebuild build-vm --impure --flake .#deployment-test

./result/bin/run-nixos-vm -nographic
```

Leave the serial console with `Ctrl-a` then `x`.

The guest is destroyed on exit. `nixos.qcow2` is not. The next run reuses it, including Postgres and the Sqitch history. Rebuilding the VM script does not delete that file. To boot a fresh root filesystem, remove it and start again:

```bash
rm -f nixos.qcow2
./result/bin/run-nixos-vm -nographic
```

Do not delete the Mithril snapshot to reset Postgres. The snapshot is the chain database, and the `chmod` above has to be repeated only if a new download replaces it.
