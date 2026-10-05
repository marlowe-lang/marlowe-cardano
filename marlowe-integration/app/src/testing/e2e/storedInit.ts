import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import { stringify as jsonStringify } from "@konduit/codec/json";
import type { PostContractSourceResponse } from '@marlowe-lang/runtime/client';
import { AddressBech32 as AB, AddressBech32 } from '@konduit/konduit-consumer/cardano';

import { Bet } from '../contracts/bet.js';
import type { Path } from '../../exec.js';
import type { JsonError } from '@konduit/codec/json/codecs';
import type { MarloweRuntimeConfig } from '../../marloweRuntimeCli.js';
import { POSIXMilliseconds } from '@konduit/konduit-consumer/time/absolute';
import { Hours, Milliseconds, Minutes, Seconds } from '@konduit/konduit-consumer/time/duration';
import { unwrapOrPanic } from '@konduit/konduit-consumer/neverthrow';

const PLACEHOLDER_ADDR = 'addr_test1vq2apcdfv7y9tm6gc2090th8747vtqmhu6vemjh0uzzu7pgcy9gsn' as AB;

// Stored contract init:
// * upload the bet contract to the store
// * init the contract using the source id
export const run = async (
  runtime: MarloweRuntimeConfig,
  fundingAddress: AddressBech32,
  tempDir: Path,
): Promise<void> => {
  const timeout = POSIXMilliseconds.bigIntCodec.serialise(unwrapOrPanic(
    POSIXMilliseconds.addMilliseconds(
      POSIXMilliseconds.now(),
      Milliseconds.fromSeconds(Seconds.fromMinutes(Minutes.fromHours(Hours.fromDigits(6))))
    ),
    `Failed to compute contract timeout: now + 6 hours`,
  ));
  const contract = Bet(
    1_000_000n,
    0n,
    PLACEHOLDER_ADDR,
    PLACEHOLDER_ADDR,
    PLACEHOLDER_ADDR,
    timeout
  );
  const bundle = [{ label: 'main', type: 'contract', value: contract }];

  const result = marloweRuntimeCli.runUploadContractSource(
    bundle,
    'main',
    runtime,
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
      runtime,
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
