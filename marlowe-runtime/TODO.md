# Extend the contract transactions endpoint

## Task

* Currently `cabal run marlowe-runtime:cli -- contract --help` does not expose transactions endpoint.

* It seems from our codebase (`Language.Marlowe.Runtime.Web.Contract.API`) that there is also a `transactions` endpoint.

* We want to expose it through the CLI as well (extend the client itself if needed).

* Please read the whole CLI package to understand the structure. Please **follow** the existing patterns and conventions.

## The devel cycle

* For quick type-checking specific package please use: `bash cabal-fast.sh typecheck [PACKAGE]` . In general this should be our main devel cycle's step - it uses `dist-O0-repl` build dir.

* For quick builds please use: `bash cabal-fast.sh build [PACKAGE]` . It uses `dist-O0` build dir.

* You can also run with `bash cabal-fast.sh run [PACKAGE]` to run the package. This is again using `dist-O0` build dir and disabled optimizations.

* WARNING: There is one package which will break completely with any of the above commands if you try to build it directly - `marlowe-binaries`. This one should be build directly only with regular `cabal build marlowe-binaries` or `cabal build lib:marlowe-binaries` etc. But you won't be doing any changes like that hopefully.


## Testing

* The local runtime is running so you can for example invoke:

  ```bash
  $ cabal run -v0 --project-file /home/paluh/projects/marlowe/marlowe-plutus/cabal.project marlowe-runtime:cli -- contract get --contract-id 'fcaa7b696aad98f05963f51348b4b8852d20518d8c8597e5a534738cc4f4c7d6#1' --message-format 'json'
  ```

* Please use the above contract to test your new `transactions` subcommand.
