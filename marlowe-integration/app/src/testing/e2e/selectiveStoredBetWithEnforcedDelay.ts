import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import * as cardanoCli from '../../cardanoCli.js';
import { unwrapOrPanic, unwrapOrPanicWith } from '@konduit/konduit-consumer/neverthrow';
import type { Json } from "@konduit/codec/json";
import { stringify as jsonStringify } from "@konduit/codec/json";
import { Bet, WinningChoice } from '../contracts/bet.js';
import {
  applyInput,
  NOTIFY_INPUT,
  waitForContractClose,
  waitForNext,
} from './bet.js';
import { findFirstNonMerkleizedCase } from '../store/selectiveMerkleization.js';
import { Choice as ChoiceAction, Contract } from '@marlowe-lang/language/v1';
import type { Wallet } from '../../cardano.js';
import type { ContractSourceId, ContractId } from '@marlowe-lang/runtime/client';
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
  tempDir: string;
};

// Walks a `Contract` depth-first to locate its `Choice` action. The bet
// contract has exactly one, so the first one we find is the one to
// preserve during selective merkleization.
const findChoiceAction = (contract: Contract): ChoiceAction | null => {
  if (contract === 'close') return null;
  if ('when' in contract) {
    for (const c of contract.when) {
      if ('case' in c && 'choose_between' in c.case) {
        return c.case;
      }
      if ('then' in c) {
        const found = findChoiceAction(c.then);
        if (found) return found;
      }
    }
    return findChoiceAction(contract.timeout_continuation);
  }
  if ('pay' in contract) return findChoiceAction(contract.then);
  if ('if' in contract) {
    return findChoiceAction(contract.then) ?? findChoiceAction(contract.else);
  }
  if ('let' in contract || 'assert' in contract) return findChoiceAction(contract.then);
  return null;
};

// Same JSON-vs-runtime semantics used in `fullMerkleization.ts`: the
// runtime serialises `bigint` as a plain JSON number, so a `bigint`/number
// pair with the same value compares equal.
function structuralEqual(a: unknown, b: unknown): boolean {
  if (a === b) return true;
  if (
    (typeof a === 'bigint' && typeof b === 'number') ||
    (typeof a === 'number' && typeof b === 'bigint')
  ) {
    return BigInt(a as number | bigint) === BigInt(b as number | bigint);
  }
  if (typeof a !== typeof b) return false;
  if (a === null || b === null) return a === b;
  if (typeof a !== 'object') return false;
  if (Array.isArray(a)) {
    if (!Array.isArray(b)) return false;
    if (a.length !== b.length) return false;
    return a.every((x, i) => structuralEqual(x, b[i]));
  }
  if (Array.isArray(b)) return false;
  const aKeys = Object.keys(a as Record<string, unknown>).sort();
  const bKeys = Object.keys(b as Record<string, unknown>).sort();
  if (aKeys.length !== bKeys.length) return false;
  if (!aKeys.every((k, i) => k === bKeys[i])) return false;
  return aKeys.every(k =>
    structuralEqual(
      (a as Record<string, unknown>)[k],
      (b as Record<string, unknown>)[k],
    ),
  );
}

const getContract = (id: ContractSourceId, runtime: MarloweRuntimeConfig): Contract => {
  const json = unwrapOrPanicWith(
    marloweRuntimeCli.runGetContractSource(id, runtime, { expand: false }, null, true),
    (err): string => `Failed to GET continuation contract source: ${jsonStringify(err as Json)}`,
  );
  return json as Contract;
};

// Asserts the contract source is still selectively merkleized: at least one
// case is preserved inline, and the preserved action matches what we asked
// the runtime to keep. The pre-lifecycle check uses `preservedAction` (the
// one we built from the original contract) for the equality check; the
// post-lifecycle check only confirms preservation, since the deployed
// instance's state has moved on.
const assertSelectivelyMerkleized = (
  sourceId: ContractSourceId,
  label: string,
  runtime: MarloweRuntimeConfig,
  expectedAction?: ChoiceAction,
): void => {
  const root = getContract(sourceId, runtime);
  const preserved = findFirstNonMerkleizedCase(root, (id: ContractSourceId) => getContract(id, runtime)) as Json | null;
  if (!preserved) {
    throw new Error(
      `[${label}] Expected the contract source to remain selectively merkleized ` +
      `(at least one non-merkleized case), but every case was merkleized. ` +
      `Source: ${jsonStringify(root, undefined, 2)}`,
    );
  }
  console.log(`[${label}] preserved case: ${jsonStringify(preserved, undefined, 2)}`);
  if (expectedAction !== undefined) {
    const preservedCase = (preserved as { case: Json }).case;
    if (!structuralEqual(preservedCase, expectedAction)) {
      throw new Error(
        `[${label}] The preserved case in the contract source does not match the action we asked to keep. ` +
        `Expected: ${jsonStringify(expectedAction)} ` +
        `Got: ${jsonStringify(preservedCase)}`,
      );
    }
  }
};

// Poll the runtime until the contract is queryable (i.e. not a 404). This
// guards against the indexer being behind the local node: a tx may be on
// chain via our local cardano-cli while the runtime's indexer has not yet
// observed it, in which case POST /contracts/{id}/transactions returns
// `LoadMarloweContextErrorNotFound`.
const waitForRuntimeContractAvailable = async (
  contractId: ContractId,
  runtime: MarloweRuntimeConfig,
  timeoutMs = 60_000,
  everyMs = 2_000,
): Promise<void> => {
  const start = Date.now();
  let attempt = 0;
  while (true) {
    attempt += 1;
    const res = marloweRuntimeCli.runGet(contractId, runtime, {}, null, true);
    const ok = res.isOk();
    console.log(`[ssbwd] waitForRuntimeContractAvailable attempt=${attempt} contractId=${contractId} ok=${ok} elapsedMs=${Date.now() - start}`);
    if (ok) return;
    if (Date.now() - start > timeoutMs) {
      throw new Error(
        `[ssbwd] runtime never acknowledged contractId=${contractId} after ${timeoutMs}ms`,
      );
    }
    await new Promise<void>((resolve) => setTimeout(resolve, everyMs));
  }
};

// Init the bet contract from a previously uploaded source by id and wait
// for the runtime to report party1's deposit as the next applicable input.
const initBetContractFromSource = async (opts: {
  party1: Wallet;
  faucet: Wallet;
  tempDir: string;
  contractSourceId: ContractSourceId;
  runtime: MarloweRuntimeConfig;
}): Promise<ContractId> => {
  const { contractSourceId, faucet, party1, tempDir, runtime } = opts;
  console.log(`[ssbwd] init-step-1: ask runtime to build init tx from source ${contractSourceId}`);
  const initResponse = unwrapOrPanicWith(
    marloweRuntimeCli.runInitBySource(
      contractSourceId,
      faucet.addr,
      runtime,
      { outputDir: tempDir },
      null,
      true,
    ),
    (err): string => `[ssbwd] init-step-1 failed: ${jsonStringify(err as Json)}`,
  );
  const contractId = initResponse.contractId;
  console.log(`[ssbwd] init-step-2: runtime returned contractId=${contractId}`);

  const signedTx = unwrapOrPanicWith(
    cardanoCli.signTxEnvelope(faucet.skeyFile, initResponse.tx, true),
    (err): string => `[ssbwd] init-step-2 sign failed: ${jsonStringify(err as Json)}`,
  );
  console.log(`[ssbwd] init-step-3: submit init tx envelope`);
  const submitResult = await cardanoCli.submitTxEnvelope(signedTx, true);
  const submitOk = submitResult.isOk();
  const initTxId = submitOk ? submitResult._unsafeUnwrap() : null;
  console.log(`[ssbwd] init-step-3 result: ok=${submitOk} txId=${initTxId}`);
  if (!submitOk) {
    throw new Error(
      `[ssbwd] init-step-3 submit failed: ${jsonStringify(submitResult._unsafeUnwrapErr() as Json)}`,
    );
  }

  console.log(`[ssbwd] init-step-4: wait for runtime to acknowledge contractId=${contractId}`);
  await waitForRuntimeContractAvailable(contractId, runtime);

  console.log(`[ssbwd] init-step-5: waitForNext (party1 deposit applicable)`);
  const nextResult = await waitForNext({
    contractId,
    party: party1,
    kind: 'deposit',
    logLabel: 'after-selective-stored-bet-with-delay-init',
    runtime,
  });
  const nextOk = nextResult.isOk();
  console.log(`[ssbwd] init-step-5 result: ok=${nextOk}`);
  if (!nextOk) {
    throw new Error(
      `[ssbwd] init-step-5 waitForNext failed: ${jsonStringify(nextResult._unsafeUnwrapErr() as Json)}`,
    );
  }
  return contractId;
};

// E2E flow of the bet contract over a selectively merkleized source
// when the bet is built with `delayOracleChoiceOnChain=true`:
//
//   1. build the bet contract (with delay) using the real wallet addresses
//   2. locate the bet's `Choice` case so we can ask the runtime to keep
//      it inline during merkleization. The `Notify` case added by the
//      delay stays merkleized on purpose — it gets resolved by the
//      runtime through the store.
//   3. upload the bundle with `preserveActions = [preservedChoice]`
//   4. assert the source is selectively merkleized (a preserved case is
//      present and matches the action we asked for)
//   5. init the contract by source id and wait for party1's deposit
//   6. apply party1's deposit, then party2's, then the oracle's choice
//   7. wait for the post-choice `Notify` to become applicable and apply it
//      — this is the additional step introduced by the delay; it has
//      to be served from the merkleized continuation
//   8. wait for the contract to close
//   9. assert the source is still selectively merkleized after the lifecycle
//
// The deposit/choice inputs are built from the `Bet` contract via the
// helpers in {@link Bet} (see `../contracts/bet.ts`).
export const run = async (opts: RunOpts): Promise<void> => {
  const { runtime, amount, oracleFee, party1, party2, oracle, faucet, winningChoice, tempDir } = opts;
  const timeout = POSIXMilliseconds.bigIntCodec.serialise(unwrapOrPanic(
    POSIXMilliseconds.addMilliseconds(
      POSIXMilliseconds.now(),
      Milliseconds.fromSeconds(Seconds.fromMinutes(Minutes.fromHours(Hours.fromDigits(6))))
    ),
    `Failed to compute contract timeout: now + 6 hours`,
  ));
  // 1) Build the contract with the real wallet addresses and the
  //    on-chain oracle-choice delay.
  const contract = Bet(
    amount,
    oracleFee,
    party1.addr,
    party2.addr,
    oracle.addr,
    timeout,
    true
  );

  // 2) Locate the Choice case we want to keep inline during
  //    merkleization. The Notify case (introduced by
  //    `delayOracleChoiceOnChain=true`) stays merkleized on purpose:
  //    the point of this scenario is to exercise the bet with the
  //    on-chain delay, where the post-settlement notify lives in a
  //    merkleized continuation and is applied via the runtime's
  //    follow-the-hash mechanism.
  const preservedChoice = findChoiceAction(contract);
  if (!preservedChoice) {
    throw new Error('Expected to find a Choice action in the bet contract to preserve');
  }

  // 3) Upload with selective merkleization (preserve only the Choice).
  const bundle = [{ label: 'main', type: 'contract', value: contract }];
  const uploaded = unwrapOrPanicWith(
    marloweRuntimeCli.runUploadContractSource(
      bundle,
      'main',
      runtime,
      { preserveActions: [preservedChoice] },
      null,
      true,
    ),
    (err): string => `Failed to upload selectively merkleized bet-with-delay bundle: ${jsonStringify(err as Json)}`,
  );
  if (!uploaded.contractSourceId || uploaded.contractSourceId.length !== 64) {
    throw new Error(`Expected a 64-hex-char contractSourceId, got: ${uploaded.contractSourceId}`);
  }

  // 4) Confirm the source is selectively merkleized: at least one
  //    preserved case is present inline, and it matches the first
  //    action we asked the runtime to keep. (The pre-lifecycle
  //    assertion is intentionally narrow: by contract the runtime
  //    picks the first preserved action it sees when traversing
  //    depth-first, and the bet guarantees the Choice is hit first.)
  assertSelectivelyMerkleized(
    uploaded.contractSourceId,
    'after-upload',
    runtime,
    preservedChoice,
  );

  // 5) Init from source and wait for the runtime to catch up.
  const contractId = await initBetContractFromSource({
    party1,
    faucet,
    tempDir,
    contractSourceId: uploaded.contractSourceId,
    runtime,
  });

  // 6) + 7) Full lifecycle: party1 deposit → party2 deposit → oracle
  // choice → notify (the new step) → close.
  console.log(`[ssbwd] lifecycle-step-1: party1 deposit`);
  const result = await applyInput({
      contractId,
      input: Bet.mkFirstDepositInput(contract),
      party: party1,
      logLabel: 'selective-bet-with-delay-after-party1-deposit',
      runtime,
    })
    .andThen(contractIdAfter => {
      console.log(`[ssbwd] lifecycle-step-1b: waitForNext (party2 deposit applicable)`);
      return waitForNext({
        contractId: contractIdAfter,
        party: party2,
        kind: 'deposit',
        logLabel: 'selective-bet-with-delay-after-party1-deposit',
        runtime,
      }).map(() => contractIdAfter);
    })
    .andThen(contractIdAfter => {
      console.log(`[ssbwd] lifecycle-step-2: party2 deposit`);
      return applyInput({
        contractId: contractIdAfter,
        input: Bet.mkSecondDepositInput(contract),
        party: party2,
        logLabel: 'selective-bet-with-delay-after-party2-deposit',
        runtime,
      }).andThen(contractIdAfter2 => {
        console.log(`[ssbwd] lifecycle-step-2b: waitForNext (oracle choice applicable)`);
        return waitForNext({
          contractId: contractIdAfter2,
          party: oracle,
          kind: 'choice',
          logLabel: 'selective-bet-with-delay-after-party2-deposit',
          runtime,
        }).map(() => contractIdAfter2);
      });
    })
    .andThen(contractIdAfter => {
      console.log(`[ssbwd] lifecycle-step-3: oracle choice`);
      return applyInput({
        contractId: contractIdAfter,
        input: Bet.mkOracleChoiceInput(contract, winningChoice),
        party: oracle,
        logLabel: 'selective-bet-with-delay-after-oracle-choice',
        runtime,
      }).andThen(contractIdAfter2 => {
        console.log(`[ssbwd] lifecycle-step-3b: waitForNext (notify applicable)`);
        return waitForNext({
          contractId: contractIdAfter2,
          party: null,
          kind: 'notify',
          logLabel: 'selective-bet-with-delay-after-oracle-choice',
          runtime,
        }).map(() => contractIdAfter2);
      });
    })
    .andThen(contractIdAfter => {
      console.log(`[ssbwd] lifecycle-step-4: notify (the on-chain delay step)`);
      // Notify has no associated party, so we use the oracle wallet
      // for the user-wallet-address + signing — the notify itself is
      // a global action that any wallet can submit.
      return applyInput({
        contractId: contractIdAfter,
        input: NOTIFY_INPUT,
        party: oracle,
        logLabel: 'selective-bet-with-delay-after-notify',
        runtime,
      }).andThen(contractIdAfter2 => {
        console.log(`[ssbwd] lifecycle-step-4b: waitForContractClose`);
        return waitForContractClose({
          contractId: contractIdAfter2,
          logLabel: 'selective-bet-with-delay-after-notify',
          runtime,
        });
      });
    });

  result.match(
    (finalState) => { console.log("selectiveStoredBetWithEnforcedDelay: final state:", finalState); },
    (error: unknown) => {
      if (typeof error === "object" && error !== null && "stderr" in error) {
        console.error((error as { stderr: unknown }).stderr);
      } else {
        console.error(error);
      }
      throw new Error(`Selective stored bet-with-delay run failed: ${jsonStringify(error as Json)}`);
    },
  );

  // 8) Final check: the contract source is still selectively merkleized
  //    after the lifecycle. The source itself is static in the store;
  //    this just guards against regressions in the upload path.
  assertSelectivelyMerkleized(
    uploaded.contractSourceId,
    'after-lifecycle',
    runtime,
  );
};
