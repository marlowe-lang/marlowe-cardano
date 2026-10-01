# Test selective merkleization

## The context

* Our marlowe-runtime server provides an ability to store, merkleize and retrieve chunks of the contract as it is executed. Additionally the contract upload API exposes a "action preservation" capability which allows to preserve selectively the chunk of the contract so a larger piece is visible on the chain. We want to test that feature.

* The test suite which lives here in `marlowe-integration/app/testing/store/` already 

