
## Data flow across indexer components

NodeFollower creates entries of type `Changes`:

```
-- | Describes a batch of chain data changes to write.
-- | IMPORTANT: Blocks are stored in reverse order (newest first).
data Changes = Changes
  { changesRollback :: !(Maybe RollbackToBlock)
  -- ^ Point to rollback to before writing any blocks.
  , changesBlocks :: ![BlockInMode]
  -- ^ New blocks to write.
  , changesTip :: !NodeTip
  -- ^ Most recently observed tip of the local node.
  , changesIndexerTip :: !IndexerTip
  -- ^ Chain tip the changes will advance the local state to.
  , changesBlockCount :: !Int
  -- ^ Number of blocks in the change set.
  , changesTxCount :: !Int
  -- ^ Number of transactions in the change set.
  }
```

It exposes them using `STM Changes` but it resets them to "empty changes" after the are read.

Currently the exposed `STM` procedure blocks till the changes are not empty which seems plain wrong because we can not observe empty blocks for example.

* `MarloweChainFollower` reads the changes and filters out blocks which are Marlowe related and emits them at once if they are not empty.

* This is picked up by the `Store` which then inserts all the changes.

We should probably c

