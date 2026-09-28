# Current State of Merkleization in `marlowe-runtime`

## 1. What already exists in the migrated `marlowe-runtime`

### Dependencies (`marlowe-runtime/marlowe-runtime.cabal`)

* `marlowe-object` (`cabal:110`) — already in the library.
* `marlowe-plutus` (`cabal:111`) — already in the library; this is the package
  that exposes the real merkleization primitives (`shallowMerkleize`,
  `deepMerkleize`, `merkleize`, `merkleizeInputs`, `dataHash`, …) at
  `marlowe-plutus/src/Marlowe/Plutus/Merkle.hs`.
* **`marlowe-contract-store` is NOT a dependency** of the library. It is
  listed in the workspace (`cabal.project:21`) and the package is built, but
  nothing in `marlowe-runtime` imports it. Adding it back will require a
  one-line `build-depends` entry.
* `marlowe-cli` is depended on only by the `server` executable
  (`cabal:270`), not by the library. It already exposes
  `merkleize`/`merkleizeMarlowe`/`demerkleize` against `Marlowe.Plutus.Merkle`
  (`marlowe-cli/src/Language/Marlowe/CLI/Merkle.hs:13-83`).

### Web API types (`marlowe-runtime/src/Language/Marlowe/Runtime/Web/Contract/API.hs`)

The merkleization-facing **shapes** are already wired in at the type level:

| Type / endpoint                                                                                              | Status                                                                                                          |
| ------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------- |
| `ContractSourceId` (= `DatumHash`, 32 raw bytes, base16)                                                     | Defined (`Contract/API.hs:299-318`) and used by `ContractOrSourceId` (`Contract/API.hs:320-342`). |
| `ContractOrSourceId = Either Contract ContractSourceId`                                                     | Defined (`Contract/API.hs:320-342`); `Arbitrary` instance at `Gen.hs:250-257`. |
| `PostContractSourcesAPI = QueryParam "main" Label :> StreamBody NewlineFraming JSON (Producer ObjectBundle …)` | Type defined (`Contract/API.hs:224-232`), response shape `PostContractSourceResponse` at `:281-285`.            |
| `GetContractSourceAPI = QueryFlag "expand" :> Get '[JSON] Contract`                                          | Type defined (`Contract/API.hs:234-238`).                                                                       |
| `GetContractSourceIdsAPI` (used for `/adjacency` and `/closure`)                                            | Type defined (`Contract/API.hs:240`).                                                                            |
| `ContractSourcesAPI = PostContractSourcesAPI :<|> Capture … :> ContractSourceAPI`                            | Type defined (`Contract/API.hs:204-206`).                                                                       |

But:

* The `ContractsAPI` aliases `ContractSourcesAPI` and `GetContractsAPI` are
  **commented out** in `Contract/API.hs:129-130`, so they are not exposed by
  the Servant server yet.
* `ContractState` (`Contract/API.hs:247-263`) stores
  `initialContract :: Semantics.Contract` as a *value*, not a hash, even
  though `InitContract` accepts `Either Contract DatumHash`
  (`Server/Monad.hs:174-183`).

### Server dependencies (`Server/Monad.hs`)

`ServerDependencies` (`Server/Monad.hs:241-253`) has **no** merkleization
dependencies. Compare with the old runtime's `WebServerDependencies`
(`.external-references/marlowe-cardano/marlowe-runtime-web/server/Language/Marlowe/Runtime/Web/RuntimeServer.hs:269-299`),
which has `_importBundle :: ImportBundle (AppM r s)` and an entire
"Merkleization and Marlowe Object" section.

The new `ServerDependencies` is missing:

* `importBundle :: Label -> Pipe ObjectBundle …`
* `getContract :: DatumHash -> m (Maybe ContractWithAdjacency)`
* a `MerkleizeInputs` query client
* a `ContractStore`-shaped record

The signature on `InitContract` (`Server/Monad.hs:174-183`) already takes
`Either (Contract V1) DatumHash`, so the type system is ready for a
hash-based initialization; the implementation in
`Contract/Server.hs:103-120` just does `DatumHash . unContractSourceId <$>
contract'` and forwards — i.e. it stores the hash without ever resolving it.

### `marlowe-transactions`

* `data InitError` already carries `InitContractNotFound String`
  (`marlowe-transactions/src/Language/Marlowe/Runtime/Transaction/Api.hs:1556`).
* The `Right hash` branch of `Init` literally throws
  `throwE (InitContractNotFound "Not ported yet")`
  (`marlowe-transactions/src/Language/Marlowe/Runtime/Transaction/Builders.hs:126-128`).
  The commented-out implementation above it shows what the function *used*
  to do (`getContract' hash` against a `ContractQueryConnector`).
* `buildApplyInputsConstraintsV1` is called with a stub
  `merkleizeInputsStub = const $ pure Nothing`
  (`Builders.hs:431-433`) instead of the real `merkleizeInputs` query client.
* `getContractContinuations` is a stub returning `pure (Just mempty)`
  (`Builders.hs:332-337`), with the real traversal of
  `MerkleizedCase`/`Case`/`When`/`If`/`Pay`/`Let`/`Assert` in commented-out
  code (`Builders.hs:338-353`).

### Schemas / generators

* `merkleized_then` is in the OpenAPI schema
  (`Core/Semantics/Schema.hs:409-421,1418-1430`).
* The `arbitrary` instance for `Web.Tx` and `Web.PostTransactionsRequest`
  carries two `-- FIXME: This should handle merkleized input, too.`
  comments (`gen/.../Web/Gen.hs:216,267`).

So: every *type* for merkleization already exists, but every *call site* that
should resolve a `ContractSourceId` to a `Contract`, or that should
auto-merkleize an input, currently throws `InitContractNotFound "Not ported
yet"` / `pure Nothing` stubs.

## 2. What needs to be brought back

Mapping the gaps against the previous design (see
`.external-references/REPORT.md` for the old API):

| Old capability (`.external-references/marlowe-cardano`)                                                                | Where it lives today                                                                                                  | Status in `marlowe-runtime` |
| -------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- | --------------------------- |
| `MarloweLoad` protocol — streaming, node-by-node merkleization of a `Contract`, returning `DatumHash`                 | `.external-references/marlowe-cardano/marlowe-runtime/contract-api/Language/Marlowe/Protocol/Load/*` and `pullContract` in `…/Protocol/Load/Server.hs:247-331` | The protocol types are vendored in `marlowe-contract-store` (`…/api/Marlowe/ContractStore/Protocol/Load/Types.hs`). The runtime-side state machine (`pullContract` + `StageContract`/`flush`/`commit`) is **not** wired in. |
| `MarloweTransfer` protocol — upload / download `ObjectBundle`s, link and merkleize on the fly                        | Same place, plus `…/marlowe-client/src/Language/Marlowe/Runtime/Client/Transfer.hs:45-93`                              | Same — protocol types exist in `marlowe-contract-store`, the runtime-side server is not wired in. |
| `ContractRequest.MerkleizeInputs` query                                                                              | `.external-references/marlowe-cardano/marlowe-runtime/contract-api/Language/Marlowe/Runtime/Contract/Api.hs:24-28,45-51` | Not present in the new runtime. |
| `ContractStore` (in-memory / file)                                                                                   | `.external-references/marlowe-cardano/marlowe-runtime/contract/Language/Marlowe/Runtime/Contract/Store{,.Memory,.File}.hs` | Not present. There is no `ContractStagingArea`, no `stageContract`, no `flush`, no `commit`. |
| Web endpoints `POST /contracts/sources`, `GET /contracts/sources/{id}?expand`, `GET …/adjacency`, `GET …/closure`   | `.external-references/marlowe-cardano/marlowe-runtime-web/src/Language/Marlowe/Runtime/Web/Adapter/Server/ContractClient.hs` and `…/Web/Contract/Source/Server.hs` | Type-level only — the Servant API is partly commented out, no handler is wired up. |

### Sub-tasks implied

1. **Add `marlowe-contract-store` to `marlowe-runtime`'s `build-depends`** so
   the existing `MarloweLoad` / `MarloweTransfer` protocol types are
   available. No source changes needed in `marlowe-contract-store`.
2. **Introduce a `ContractStore`-shaped type** in `marlowe-runtime` (or in a
   new sibling package). It must support `getContract :: DatumHash -> m (Maybe
   Contract)`, `createContractStagingArea :: m (ContractStagingArea m)`,
   `merkleizeInputs :: Contract -> State -> TransactionInput -> m (Either
   MerkleizeInputsError TransactionInput)`. The old memory + file
   implementations (`…/Store/Memory.hs`, `…/Store/File.hs`) are good
   starting points, but for the new runtime we likely want a
   `ContractStore` that reads from the indexer (`marlowe-runtime/src/.../Query.hs`).
3. **Wire `marlowe-transactions`' `Init` to actually look up the contract by
   hash** instead of throwing `"Not ported yet"` — see the commented-out
   `getContract' hash` snippet in
   `marlowe-transactions/src/Language/Marlowe/Runtime/Transaction/Builders.hs:128-131`.
4. **Wire `merkleizeInputsStub` in `marlowe-transactions/.../Builders.hs:431-436`
   to a real `merkleizeInputs` query**, the same way
   `.external-references/marlowe-cardano/marlowe-runtime/tx/Language/Marlowe/Runtime/Transaction/Server.hs:686`
   does.
5. **Add the missing fields to `ServerDependencies`** (`Server/Monad.hs:241-253`):
   * `importBundle :: Label -> Pipe ObjectBundle (Map Label DatumHash) ServerM (Either ImportError (Map Label DatumHash))`
   * `getContract :: DatumHash -> ServerM (Maybe (Contract, Set DatumHash, Set DatumHash))`
   * possibly a `merkleizeInputs` field too, depending on where the auto-merkle
     helper lives.
6. **Re-enable the commented-out `ContractSourcesAPI` / `GetContractsAPI`
   endpoints** in `Contract/API.hs:129-130` and add handlers
   (`Contract/Server.hs:55-65` would gain two new endpoints). The
   `GetContractSourceAPI` expand path should call
   `deepDemerkleize` (currently `merkle-plutus/src/Marlowe/Plutus/Merkle.hs:131-141`)
   with the contract and the closure set, exactly as
   `.external-references/marlowe-cardano/marlowe-runtime-web/src/Language/Marlowe/Runtime/Web/Contract/Source/Server.hs:55-76`
   does.
7. **Add `marlowe-object` round-trip** through the web server: an upload
   endpoint should `linkBundle` each incoming `ObjectBundle` (already in
   `marlowe-object`), hash every contract (with `Marlowe.Plutus.Merkle.dataHash`),
   persist them, and return the resulting `Map Label ContractSourceId`.
8. **Resolve the `arbitraryNormal -- FIXME` comments** in
   `gen/Language/Marlowe/Runtime/Web/Gen.hs:216,267` so merkleized inputs can
   be generated for property tests.

## 3. Can the other dependencies be reused?

Yes — and the wiring is mostly already done at the type level:

| Dependency                  | Already used?                                              | Reusable for                                                                                                            |
| --------------------------- | ---------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| `marlowe-object`             | Yes (`Marlowe.Object.Types` imported in `Web/Client.hs:39`, `Contract/API.hs:39`, `Core/Semantics/Schema.hs:48-49`). | `ObjectBundle`, `Link`, `fromCoreCase`, `dataHash` (already imported as `Marlowe.Plutus.Merkle.dataHash`). Nothing needs to change. |
| `marlowe-plutus`             | Yes.                                                       | All the merkleization primitives (`deepMerkleize`, `merkleizeInputs`, `dataHash`) are imported from `Marlowe.Plutus.Merkle` and ready to use. The migration has not stopped any of these from being imported. |
| `marlowe-contract-store`     | Package exists in the workspace but is NOT a dependency of `marlowe-runtime`. | The protocol types (`Marlowe.ContractStore.Protocol.Load.*` and `…Transfer.*`) are exactly the wire formats the old runtime used. Adding the dependency is enough to be able to drive a streaming merkleization server. |
| `marlowe-cli`                | Yes (only in the `server` executable).                     | Already has `merkleizeMarlowe` for offline use. Probably not relevant for the runtime, but available.                     |
| `marlowe-plutus-extended`    | (used by other packages)                                   | Nothing to do with merkleization; irrelevant here.                                                                      |
| `marlowe-transactions`       | Yes.                                                       | The `Init` and `ApplyInputs` builder is the integration point — the comment "Not ported yet" + `merkleizeInputsStub` is the single biggest "wire me up" location. |

In short: **the data type and protocol shapes for merkleization are already
in the migrated codebase**. The implementation gaps are concentrated in
three places:

1. `marlowe-transactions/.../Builders.hs` (`Init`'s `Right hash` branch and
   `merkleizeInputsStub`),
2. `marlowe-runtime/.../Server/Monad.hs` and `…/Contract/Server.hs`
   (server-side wiring, especially the commented-out endpoints), and
3. a new `ContractStore`-shaped component (which the indexer-based
   `marlowe-runtime` may want to back with PostgreSQL rather than the old
   in-memory / file backend).

Adding `marlowe-contract-store` to the dependency list, replacing the
`InitContractNotFound "Not ported yet"` and `merkleizeInputsStub = const $
pure Nothing` stubs with real calls, and implementing the four
`ContractSource` web endpoints are the three concrete next moves.
