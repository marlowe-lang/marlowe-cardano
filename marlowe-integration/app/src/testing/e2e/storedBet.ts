import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import * as cardanoCli from '../../cardanoCli.js';
import { unwrapOrPanicWith } from '@konduit/konduit-consumer/neverthrow';
import type { Json } from "@konduit/codec/json";
import { stringify as jsonStringify } from "@konduit/codec/json";

import { mkContract, mkContractTimeout, WinningChoice } from './bet.js';
import {
  applyDeposit,
  applyChoice,
  waitForNext,
} from './bet.js';
import type { Wallet } from '../../cardano.js';
import type { ContractId } from '@marlowe-lang/runtime/client';

type RunOpts = {
  amount: bigint;
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
async function initBetContractFromSource(opts: {
  amount: bigint;
  party1: Wallet;
  party2: Wallet;
  oracle: Wallet;
  faucet: Wallet;
  tempDir: string;
}): Promise<ContractId> {
  const { amount, party1, party2, oracle, faucet, tempDir } = opts;
  const contract = mkContract(amount, party1.addr, party2.addr, oracle.addr, mkContractTimeout());
  const bundle = [{ label: 'main', type: 'contract', value: contract }];

  // 1) Upload the contract to the store.
  const uploaded = unwrapOrPanicWith(
    marloweRuntimeCli.runUploadContractSource(bundle, 'main', {}, null, true),
    (err): string => `Failed to upload bet bundle: ${jsonStringify(err as Json)}`,
  );
  if (!uploaded.contractSourceId || uploaded.contractSourceId.length !== 64) {
    throw new Error(`Expected a 64-hex-char contractSourceId, got: ${uploaded.contractSourceId}`);
  }

  // 2) Init the contract from the stored source.
  const initResult = marloweRuntimeCli.runInitBySource(
    uploaded.contractSourceId,
    faucet.addr,
    { outputDir: tempDir },
    null,
    true,
  );
  if (initResult.isErr()) {
    throw new Error(`Failed to init from source: ${initResult.error}`);
  }
  const initResponse = initResult.value;
  const contractId = initResponse.contractId;

  // Sign + submit the init tx, then wait for the runtime to report
  // party1's deposit as the next applicable input.
  const signed = cardanoCli.signTxEnvelope(faucet.skeyFile, initResponse.tx, true);
  if (signed.isErr()) {
    throw new Error(`Failed to sign init tx: ${signed.error}`);
  }
  const submitted = cardanoCli.submitTxEnvelope(signed.value, true);
  await submitted.andThen(() =>
    waitForNext({ contractId, party: opts.party1, kind: 'deposit', logLabel: 'after-stored-init' }),
  );
  return contractId;
}

// Same flow as bet.ts, but init goes through the store. The point of this
// scenario is to prove the input-application path also works when the
// `When` is supplied via the store rather than the init tx payload.
export const run = async (opts: RunOpts & { tempDir: string }): Promise<void> => {
  const { amount, party1, party2, oracle, faucet, winningChoice, tempDir } = opts;
  const contractId = await initBetContractFromSource({
    amount,
    party1,
    party2,
    oracle,
    faucet,
    tempDir,
  });
  const result = await applyDeposit({
      contractId: contractId,
      party: party1,
      amount,
      nextParty: { wallet: party2, kind: 'deposit' },
      logLabel: 'after-party1-deposit',
    })
    .andThen(() => applyDeposit({
      contractId: contractId,
      party: party2,
      amount,
      nextParty: { wallet: oracle, kind: 'choice' },
      logLabel: 'after-party2-deposit',
    }))
    .andThen(() => applyChoice({
      contractId: contractId,
      party: oracle,
      choiceValue: WinningChoice.toChoiceValue(winningChoice),
    }));

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
