# Test selective merkleization

* We have a few scenarios of contracts execution in the ` ./app/src/testing/e2e/` directory.

* We have a specific version of bet which we upload to store and do a selective merkleization over it in `./app/src/testing/store/selectiveBet.ts` (actually this should be renamed to `selectiveMerkleization.ts` and `store/bet.ts` should be renamed to `store/fullMerkleization.ts`).

* All test are executed from the ` ./app/tests/e2e/runtimeCli.test.ts` (which should be renamed to `tests/run.tests.ts` probably or `tests/index.test.ts`).

* Now we want to actually add e2e scenario which uses that selectively merkleized bet:

  * Please check all the other `e2e` scenarios for inspiration.

  * Please check that selective merkleization test as well.

  * Our test should repeat some checks - it should confirm that the selective merkleization is actually in place.

* The local testnet is running and the runtime is part of it exposed on the standard 8090 port.
