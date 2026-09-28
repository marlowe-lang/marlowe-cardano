# Improve the `bet.ts` test case

## Review

* I noticed that we can actually drop some type aliases definitions and use existing ones from `./marlowe-integration/packages/language` if possible. At least `ChoiceId` and `Bounds` are two which I spotted.

* I would like to preserve makeDeposit and makeChoice functions to be generic... I would actually investigate `next` result before executing the deposit for a given party. The function could accept a flag like `awaitTillApplicable` which would indicate if the function should await till the server allows us to perform the step if it is false we should just fail the test case if `next` returns something incompatible with the expected input.



---------------------------------------------------------------------------------------------------------------------------------------------

<!-- The original description of the problem -->

The bet test case is *nearly* working. There is one problem though and it is about input application. If after first deposit we try to apply a second deposit too quickly before the runtime spots our previous transaction submission (through its chain indexer) it rejects it because it is still in the previous steps and only allows to create the first deposit (which we know does not make sense because we already submitted it).

## The task

* Please check the `./marlowe-integration/app/openapi.json` which is the current open API specification for the Marlowe runtime server.

* In this specification you can find `/contracts/{contractId}/next` and `"Next"` type.

* We want to create a simple wiring for data types for that endpoint with basic codecs in the `./marlowe-integration/packages/runtime/src/client/contracts/next.ts` we should probably also rename the `./marlowe-integration/packages/runtime/src/client/contract.ts` to `contracts.ts` to reflect the API naming.

* Then we want to also extend the `Client.hs` from `./marlowe-runtime/src` so it exposes a `next function.

* Then we want to add support for that function to the `./marlowe-runtime/cli` similarly to the other client commands.

* Then we want to wire that up in the `./marlowe-integration/app/src/marloweRuntimeCli.ts`.

* Then we want to use that next function in our `bet` flow and actually await till the result from it awaits for the next expected input - second deposit and subsequently the choice.


