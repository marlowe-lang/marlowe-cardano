# Bet e2e with merkleization

We have the `bet` test case scenario and we have `store` test cases which test the merkleization and storage of the contracts on the runtime side. We want to create a test and wire up the input application with store. It was there in the past but during migration we lost it (it was commented out). In essence during input application the rest of the contract in the case of MerkleizedCase could be looked up in the store and used.

But first we will create a test scenario which just initializes stored contract. Later we will move to the full `bet` flow.

## The task

* Please create a test scenario `stored-init` for the `bet` flow which first uploads the contract into the store.

* We now want to deploy that contract by using only its id. I hope that this path was preserved but commented out in the runtime.

* Next we want to move to a full e2 `stored-bet` flow.

* This should be similar to the `bet` but push the contract into the store and then do the whole input application which should just work if the runtime fixes will be applied correctly.
