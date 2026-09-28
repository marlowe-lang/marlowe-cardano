import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import { stringify as jsonStringify } from "@konduit/codec/json";
import type { PostContractSourceResponse } from '@marlowe-lang/runtime/client';
import { AddressBech32 as AB, AddressBech32 } from '@konduit/konduit-consumer/cardano';

import { mkContract, mkContractTimeout } from '../e2e/bet.js';
import type { Path } from '../../exec.js';
import type { JsonError } from '@konduit/codec/json/codecs';

const PLACEHOLDER_ADDR = 'addr_test1vq2apcdfv7y9tm6gc2090th8747vtqmhu6vemjh0uzzu7pgcy9gsn' as AB;

// Stored contract init:
// * upload the bet contract to the store
// * init the contract using the source id
export const run = async (fundingAddress: AddressBech32, tempDir: Path): Promise<void> => {
  const contract = mkContract(1_000_000n, PLACEHOLDER_ADDR, PLACEHOLDER_ADDR, PLACEHOLDER_ADDR, mkContractTimeout());
  const bundle = [{ label: 'main', type: 'contract', value: contract }];

  const result = marloweRuntimeCli.runUploadContractSource(
    bundle,
    'main',
    {},
    null,
    true,
  ).andThen((response: PostContractSourceResponse) => {
    // Init the contract from the store by id. The runtime CLI hits
    // `contract init --contract-source-id <id>` and the runtime server resolves
    // the source from its store.
    return marloweRuntimeCli.runInitCLI(
      { kind: 'source', contractSourceId: response.contractSourceId },
      fundingAddress,
      { outputDir: tempDir },
      null,
      true,
    );
  });
  result.match(
    (initResult) => {
      console.log('Stored contract init succeeded:', jsonStringify(initResult));
    },
    (err: JsonError) => {
      throw new Error(`Stored contract init failed:, ${jsonStringify(err)}`);
    },
  );
};



