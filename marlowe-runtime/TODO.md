# Extend the contract transactions endpoint

## Task

* In the recent iteration we added `cabal run marlowe-runtime:cli -- contract transactions --help` (please check the previous commit diff for the `marlowe-runtime/client` and relevant changes across `marlowe-runtime`).

* We want to now extend the command so with a `--full` flag which will internally iterate over the transactions and fetch the full transaction details and use them to produce a more detailed output.

## The devel cycle

* For quick type-checking specific package please use: `bash cabal-fast.sh typecheck [PACKAGE]` . In general this should be our main devel cycle's step - it uses `dist-O0-repl` build dir.

* For quick builds please use: `bash cabal-fast.sh build [PACKAGE]` . It uses `dist-O0` build dir.

* You can also run with `bash cabal-fast.sh run [PACKAGE]` to run the package. This is again using `dist-O0` build dir and disabled optimizations.

* WARNING: There is one package which will break completely with any of the above commands if you try to build it directly - `marlowe-binaries`. This one should be build directly only with regular `cabal build marlowe-binaries` or `cabal build lib:marlowe-binaries` etc. But you won't be doing any changes like that hopefully.


## Testing

* The local runtime is running so you can for example invoke:

  ```bash
  $ cabal run marlowe-runtime:cli -- contract transactions --contract-id '11073b55075924157aa34f365ef085ec8570440662d343165cc8880900af2f7a#1' --message-format 'json'
  ```

* Please use that exact contract id to test your new `transactions` with details output.

