# marlowe-runtime-cli

A Command line interface for the Marlowe Runtime.

## Commands

### `log`

Show the history of a contract instance.

### `apply`

Apply inputs to a contract instance.

### `advance`

Advance a timed-out contract instance.

### `deposit`

Deposit funds into a contract instance account.

### `choose`

Make a choice in a contract instance.

### `notify`

Advance a contract instance waiting on a notification.

### `create`

Create a new contract instance.

### `withdraw`

Withdraw funds paid to a contract role.

### `submit`

Submit a signed transaction to the connected cardano-node.

### `load`

Incrementally load a contract into the runtime's contract store.

#### `load` file format

The `load` command accepts a filepath to a contract file. The file is a JSON
(or YAML) representation of a contract. The file format differs from the
standard Marlowe contract JSON schema in two important ways: support for
imports, and lack of merkleization.

Any place where a contract or subcontract appears can be replaced by an import,
which looks like this:

```json
{ "import": "./relative/path/to/contract/file.json" }
```

The imports are resolved relative to the directory where the importing file
resides. Note that cyclic imports are forbidden.

Furthermore, merkleization in the contract file is not allowed. This is because
the Runtime will automatically merkleize the contract and requires that all
subcontracts can be found in the store.
