# Add optional starting point for the indexer

The goal of this task is to extend our marlowe-indexer:server so it accepts two new parameters:

  * `--start-from BLOCK_HASH` which accepts a block hash and starts the indexer from that point. I'm not sure if the hash is enough to identify the block - please check `.external-references/mgdoc/mgdoc/l1-state-synchronizer` for some inspiration if needed.

  * `--start-from-genesis` which starts the indexer from the genesis block.

If none of the above parameters are provided, the indexer if run for the first time should start from the `tip` of the node. If the db contains some blocks already, then the indexer should start from the last block in the db as it does now.


## The devel cycle

* For quick type-checking specific package please use: `bash cabal-fast.sh typecheck [PACKAGE]` . In general this should be our main devel cycle's step - it uses `dist-O0-repl` build dir.

* For quick builds please use: `bash cabal-fast.sh build [PACKAGE]` . It uses `dist-O0` build dir.

* You can also run with `bash cabal-fast.sh run [PACKAGE]` to run the package. This is again using `dist-O0` build dir and disabled optimizations.

* WARNING: There is one package which will break completely with any of the above commands if you try to build it directly - `marlowe-binaries`. This one should be build directly only with regular `cabal build marlowe-binaries` or `cabal build lib:marlowe-binaries` etc. But you won't be doing any changes like that hopefully.

## Testing

Please only make sure that the project builds at the end. We will do the testing in the next iteration.
