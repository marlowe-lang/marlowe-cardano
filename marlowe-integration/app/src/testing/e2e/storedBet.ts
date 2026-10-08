import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import * as cardanoCli from '../../cardanoCli.js';
import { unwrapOrPanic, unwrapOrPanicWith } from '@konduit/konduit-consumer/neverthrow';
import type { Json } from "@konduit/codec/json";
import { stringify as jsonStringify } from "@konduit/codec/json";
import { Bet, WinningChoice } from '../contracts/bet.js';
import {
  applyInput,
  waitForContractClose,
  waitForNext,
} from './bet.js';
import type { Wallet } from '../../cardano.js';
import type { ContractId } from '@marlowe-lang/runtime/client';
import type { MarloweRuntimeConfig } from '../../marloweRuntimeCli.js';
import { POSIXMilliseconds } from '@konduit/konduit-consumer/time/absolute';
import { Hours, Milliseconds, Minutes, Seconds } from '@konduit/konduit-consumer/time/duration';

type RunOpts = {
  runtime: MarloweRuntimeConfig;
  amount: bigint;
  oracleFee: bigint;
  party1: Wallet;
  party2: Wallet;
  oracle: Wallet;
  faucet: Wallet;
  winningChoice: WinningChoice;
};

// Init the bet contract by uploading it to the store first and then
// requesting the runtime to deploy from the source id. This exercises the
// merkleized init path: the source content (including the oracle's `When`
// case) is fetched from the store rather than carried in the init tx payload.
//
// We expose the `Bet` instance via `RunOpts.bet` so the lifecycle
// callers can extract their deposit/choice inputs from the same
// contract that was uploaded.
async function initBetContractFromSource(opts: {
  contract: Bet;
  party1: Wallet;
  faucet: Wallet;
  tempDir: string;
  runtime: MarloweRuntimeConfig;
}): Promise<ContractId> {
  const { contract, faucet, party1, tempDir, runtime } = opts;
  const bundle = [{ label: 'main', type: 'contract', value: contract }];

  // 1) Upload the contract to the store.
  const uploaded = unwrapOrPanicWith(
    marloweRuntimeCli.runUploadContractSource(bundle, 'main', runtime, {}, null, true),
    (err): string => `Failed to upload bet bundle: ${jsonStringify(err as Json)}`,
  );

  // 2) Init the contract from the stored source.
  const { contractId, tx: initTx } = unwrapOrPanicWith(marloweRuntimeCli.runInitBySource(
      uploaded.contractSourceId,
      faucet.addr,
      runtime,
      { outputDir: tempDir },
      null,
      true,
    ), (err): string => `Failed to init bet from source: ${jsonStringify(err as Json)}`);

  // Sign + submit the init tx, then wait for the runtime to report
  // party1's deposit as the next applicable input.
  const signedTx = unwrapOrPanicWith(
    cardanoCli.signTxEnvelope(faucet.skeyFile, initTx, true),
    (err): string => `Failed to sign init tx: ${jsonStringify(err as Json)}`,
  );
  await cardanoCli.submitTxEnvelope(signedTx, true).andThen(() =>
    waitForNext({ contractId, party: party1, kind: 'deposit', logLabel: 'after-stored-init', runtime }),
  );
  return contractId;
}

// Same flow as bet.ts, but init goes through the store. The point of this
// scenario is to prove the input-application path also works when the
// `When` is supplied via the store rather than the init tx payload.
//
// The deposit/choice inputs are built from the `Bet` contract via the
// helpers in {@link Bet} (see `../contracts/bet.ts`).
export const run = async (opts: RunOpts & { tempDir: string }): Promise<void> => {
  const { runtime, amount, oracleFee, party1, party2, oracle, faucet, winningChoice, tempDir } = opts;
  const timeout = POSIXMilliseconds.bigIntCodec.serialise(unwrapOrPanic(
    POSIXMilliseconds.addMilliseconds(
      POSIXMilliseconds.now(),
      Milliseconds.fromSeconds(Seconds.fromMinutes(Minutes.fromHours(Hours.fromDigits(6))))
    ),
    `Failed to compute contract timeout: now + 6 hours`,
  ));
  const contract = Bet(
    amount,
    oracleFee,
    party1.addr,
    party2.addr,
    oracle.addr,
    timeout
  );
  const contractId = await initBetContractFromSource({
    contract,
    party1,
    faucet,
    tempDir,
    runtime,
  });
  const result = await applyInput({
      contractId: contractId,
      input: Bet.mkFirstDepositInput(contract),
      party: party1,
      logLabel: 'after-party1-deposit',
      runtime,
    })
    .andThen(contractIdAfter =>
      waitForNext({
        contractId: contractIdAfter,
        party: party2,
        kind: 'deposit',
        logLabel: 'after-party1-deposit',
        runtime,
      }).map(() => contractIdAfter),
    )
    .andThen(contractIdAfter =>
      applyInput({
        contractId: contractIdAfter,
        input: Bet.mkSecondDepositInput(contract),
        party: party2,
        logLabel: 'after-party2-deposit',
        runtime,
      })
      .andThen(contractIdAfter2 =>
        waitForNext({
          contractId: contractIdAfter2,
          party: oracle,
          kind: 'choice',
          logLabel: 'after-party2-deposit',
          runtime,
        }).map(() => contractIdAfter2),
      ),
    )
    .andThen(contractIdAfter =>
      applyInput({
        contractId: contractIdAfter,
        input: Bet.mkOracleChoiceInput(contract, winningChoice),
        party: oracle,
        logLabel: 'after-oracle-choice',
        runtime,
      })
      .andThen(contractIdAfter2 =>
        waitForContractClose({
          contractId: contractIdAfter2,
          logLabel: 'after-oracle-choice',
          runtime,
        }),
      ),
    );

  result.match(
    (finalState) => { console.log("stored-bet: final state:", finalState); },
    (error: unknown) => {
      if (typeof error === "object" && error !== null && "stderr" in error) {
        console.error((error as { stderr: unknown }).stderr);
      } else {
        console.error(error);
      }
      throw new Error(`Stored bet run failed: ${String(error)}`);
    },
  );
};
