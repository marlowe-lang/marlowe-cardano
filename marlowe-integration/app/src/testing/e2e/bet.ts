import type { AddressBech32 } from '@konduit/konduit-consumer/cardano';
import type { INotify, NormalInput } from '@marlowe-lang/language/v1';
import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import * as cardanoCli from '../../cardanoCli.js';
import { unwrapOrPanic, unwrapOrPanicWith } from '@konduit/konduit-consumer/neverthrow';
import type { Json } from "@konduit/codec/json";
import { stringify as jsonStringify } from "@konduit/codec/json";
import { POSIXMilliseconds } from '@konduit/konduit-consumer/time/absolute';
import { Milliseconds, Seconds, Minutes, Hours } from '@konduit/konduit-consumer/time/duration';
import { toAsync } from '@konduit/konduit-consumer/neverthrow';
import type { ResultAsync } from 'neverthrow';
import { ResultAsync as ResultAsyncCtor } from 'neverthrow';
import { waitPatientlyForResultAsync } from '../../neverthrow.js';
import type { Wallet } from '../../cardano.js';
import type { MarloweRuntimeConfig } from '../../marloweRuntimeCli.js';
import * as nodeOs from 'node:os';
import * as nodeFs from 'node:fs';
import {
  ApplyInputsResponse,
  ContractId,
  ContractState,
  Next,
  PostCreateContractResponse,
} from '@marlowe-lang/runtime/client';
import {
  Bet,
  WinningChoice,
} from '../contracts/bet.js';

export type RunOpts = {
  runtime: MarloweRuntimeConfig;
  amount: bigint;
  oracleFee: bigint;
  party1: Wallet;
  party2: Wallet;
  oracle: Wallet;
  faucet: Wallet;
  // The numeric value the oracle will submit: 0n (no winners), 1n
  // (party1 wins), or 2n (party2 wins).
  winningChoice: WinningChoice;
};

const POLL_TIMEOUT_MS = 120_000;
const POLL_EVERY_MS = 5_000;

// Helper: is the runtime reporting that `party` can either deposit or
// choose next? Used to gate each step on the runtime's chain indexer
// catching up to our previous submit.
const addressOf = (p: { address: AddressBech32 } | { role_token: string }): AddressBech32 | null =>
  'address' in p ? p.address : null;

const isApplicableFor = (next: Next, party: Wallet | null, kind: 'deposit' | 'choice' | 'notify'): boolean => {
  if (kind === 'notify') {
    // Notify is a global action with no associated party.
    return next.applicable_inputs.notify !== undefined && next.applicable_inputs.notify !== null;
  }
  if (party === null) {
    return false;
  }
  const partyAddr = party.addr;
  if (kind === 'deposit') {
    return next.applicable_inputs.deposits.some(
      (d) => addressOf(d.party) === partyAddr,
    );
  }
  return next.applicable_inputs.choices.some(
    (c) => addressOf(c.for_choice.choice_owner) === partyAddr,
  );
};

// Fetch the current `When` case's `timeout` (POSIX ms) from the runtime's
// view of the contract. We use this to set the validity range so that it
// sits unambiguously before the timeout — otherwise the validator can
// legitimately refuse the input because the validity range could span
// past the timeout and pick the timeout branch instead.
//
// Returns `null` if the contract has no active `When` (e.g. it's closed)
// or if the runtime can't be reached.
const getCurrentWhenTimeoutMs = async (
  contractId: ContractId,
  runtime: MarloweRuntimeConfig,
): Promise<bigint | null> => {
  const result = await toAsync(marloweRuntimeCli.runGet(contractId, runtime, {}, null, true));
  return result.match(
    (s: ContractState): bigint | null => {
      const current = s.currentContract as
        | { timeout?: number | string | bigint }
        | null
        | undefined;
      if (!current || current.timeout === undefined || current.timeout === null) {
        return null;
      }
      // The JSON wire format for `timeout` is a number; coerce safely.
      const t = current.timeout;
      return typeof t === 'bigint' ? t : BigInt(t);
    },
    (_e: unknown) => null,
  );
};

// Compute a validity range [fromMs, toMs] (POSIX ms) that sits entirely
// inside the active `When` case's non-timeout window. `toMs` is the
// timeout minus `SAFETY_MARGIN_MS` so the validator can prove the
// timeout has not been reached; `fromMs` is `toMs - WINDOW_MS` to
// leave a reasonable window for the runtime to advance the chain
// indexer between the init/previous submit and the next call.
const SAFETY_MARGIN_MS = 60_000n; // 1 minute before the timeout
const WINDOW_MS = 60_000n; // 1-minute validity window
const buildValidityRangeFromTimeout = (timeoutMs: bigint): { fromMs: bigint; toMs: bigint } => {
  const toMs = timeoutMs - SAFETY_MARGIN_MS;
  const fromMs = toMs - WINDOW_MS;
  return { fromMs, toMs };
};

// Logs a snapshot of the runtime's view of the contract together with
// the latest `/next` result, so we can spot discrepancies (e.g. runtime
// sees the contract but reports no applicable inputs, or the indexer has
// not caught up yet).
const logRuntimeSnapshot = async (
  label: string,
  contractId: ContractId,
  party: Wallet,
  runtime: MarloweRuntimeConfig,
): Promise<void> => {
  const stateResult = await toAsync(
    marloweRuntimeCli.runGet(contractId, runtime, {}, null, true),
  );
  const nextResult = await toAsync(
    marloweRuntimeCli.runNext(contractId, [party.addr], runtime, {}, null, true),
  );
  console.log(`[${label}] state:`, JSON.stringify(stateResult.match(
    (s: ContractState) => ({ contractId: s.contractId, hasCurrent: !!s.currentContract, hasState: !!s.state }),
    (e: unknown) => ({ error: String(e) }),
  ), null, 2));
  console.log(`[${label}] next:`, JSON.stringify(nextResult.match(
    (n: Next) => ({
      can_reduce: n.can_reduce,
      depositCount: n.applicable_inputs.deposits.length,
      choiceCount: n.applicable_inputs.choices.length,
      notify: !!n.applicable_inputs.notify,
    }),
    (e: unknown) => ({ error: String(e) }),
  ), null, 2));
};

// Format a POSIX-ms timestamp as ISO-8601 UTC for the CLI.
const toIsoUtc = (ms: bigint): string => new Date(Number(ms)).toISOString();

// Polls the runtime's `/next` endpoint until the expected input for `party`
// is reported as applicable, or times out. This is the gating step between
// submits that prevents the runtime from rejecting our next call because
// it still has stale chain state.
//
// The validity range is derived from the runtime's own view of the
// current `When` case's `timeout` (queried via `runGet`), so it sits
// unambiguously before the timeout regardless of any host/runtime clock
// skew. This is the fix for the "Invalid Interval" error where the test
// was using the test-host's `now` for the upper bound, which (after
// translating through the runtime) could fall past the contract's
// timeout and trip the validator.
export const waitForNext = (opts: {
  contractId: ContractId;
  party: Wallet | null;
  kind: 'deposit' | 'choice' | 'notify';
  logLabel: string;
  runtime: MarloweRuntimeConfig;
}): ResultAsync<Next, unknown> => {
  const runNextWithDerivedRange = async (): Promise<Next> => {
    // For notify, the runtime's /next does not need a party filter.
    const parties = opts.party !== null ? [opts.party.addr] : [];
    const timeoutMs = await getCurrentWhenTimeoutMs(opts.contractId, opts.runtime);
    if (timeoutMs === null) {
      // Contract has no active When (e.g. closed) or runtime unreachable.
      // Fall back to the default wall-clock range so we still poll.
      return unwrapOrPanicWith(
        await toAsync(
          marloweRuntimeCli.runNext(opts.contractId, parties, opts.runtime, {}, null, true),
        ),
        (err): string => `Failed to query runtime /next (no timeout): ${jsonStringify(err as Json)}`,
      );
    }
    const { fromMs, toMs } = buildValidityRangeFromTimeout(timeoutMs);
    return unwrapOrPanicWith(
      await toAsync(
        marloweRuntimeCli.runNext(
          opts.contractId,
          parties,
          opts.runtime,
          { validityStart: toIsoUtc(fromMs), validityEnd: toIsoUtc(toMs) },
          null,
          true,
        ),
      ),
      (err): string => `Failed to query runtime /next (with derived range): ${jsonStringify(err as Json)}`,
    );
  };
  let attempt = 0;
  return waitPatientlyForResultAsync(
    () => ResultAsyncCtor.fromPromise(runNextWithDerivedRange(), (e: unknown) => e),
    (next) => {
      const ready = isApplicableFor(next, opts.party, opts.kind);
      // Log a fuller snapshot every 5th attempt to help debug cases where
      // the runtime sees the contract but never reports the expected input.
      if (!ready && attempt % 5 === 0 && opts.party !== null) {
        logRuntimeSnapshot(opts.logLabel, opts.contractId, opts.party, opts.runtime);
      }
      attempt += 1;
      return ready;
    },
    { timeoutMs: POLL_TIMEOUT_MS, everyMs: POLL_EVERY_MS },
  );
};

// Initializes the bet contract via the marlowe-runtime CLI, signs the init
// transaction with the faucet wallet, submits it, and waits until the runtime
// reports `party1`'s deposit as the next applicable input.
export const initBetContract = (opts: {
  contract: Bet;
  party1: Wallet;
  faucet: Wallet;
  runtime: MarloweRuntimeConfig;
}): ResultAsync<ContractId, unknown> => {
  const { contract, faucet, party1, runtime } = opts;
  return toAsync(marloweRuntimeCli.runInit(contract, faucet.addr, runtime, {}, null, true))
    .andThen((response: PostCreateContractResponse) =>
      cardanoCli.signTxEnvelope(faucet.skeyFile, response.tx, true).map(signed => ({
        contractId: response.contractId,
        txEnvelope: signed,
      })),
    )
    .andThen(({ contractId, txEnvelope }) =>
      cardanoCli.submitTxEnvelope(txEnvelope, true).map(() => contractId),
    )
    .andThen(contractId =>
      // Wait for the runtime to see our init tx and report party1's
      // deposit as the next applicable input.
      waitForNext({
        contractId,
        party: party1,
        kind: 'deposit',
        logLabel: 'after-init',
        runtime,
      }).map(() => contractId),
    );
};

// Submits a `NormalInput` (deposit or choice) on behalf of `party` and waits
// for the runtime's chain indexer to acknowledge the new contract state.
//
// The input is built from the `Bet` contract via
// {@link Bet.mkFirstDepositInput} / {@link Bet.mkSecondDepositInput} /
// {@link Bet.mkOracleChoiceInput}; the e2e helper only orchestrates
// signing/submission/waiting. The wait-for-`next`-input / wait-for-close
// gating lives at the call sites — see `run` below for the typical
// deposit-then-choice lifecycle.
export const applyInput = (opts: {
  contractId: ContractId;
  input: NormalInput;
  party: Wallet;
  logLabel: string;
  runtime: MarloweRuntimeConfig;
}): ResultAsync<ContractId, unknown> => {
  const { contractId, input, party, logLabel, runtime } = opts;
  console.log(`[${logLabel}] applyInput: party=${party.addr} input=${jsonStringify(input as Json)}`);
  // The CLI writes the unsigned tx into `outputDir`; if we don't pass one
  // it defaults to `./out` and fails if that directory doesn't exist.
  const outputDir = `${nodeFs.mkdtempSync(`${nodeOs.tmpdir()}/marlowe-apply-`)}`;
  return toAsync(
    marloweRuntimeCli.runApplyInputs([input], contractId, party.addr, runtime, { outputDir }, null, true),
  )
    .andThen((response: ApplyInputsResponse) =>
      cardanoCli.signTxEnvelope(party.skeyFile, response.tx, true).map(signed => ({
        contractId: response.contractId,
        txEnvelope: signed,
      })),
    )
    .andThen(({ txEnvelope, contractId: newContractId }) =>
      cardanoCli.submitTxEnvelope(txEnvelope, true).map(() => newContractId),
    );
};

// The `INotify` constant from the language module is the literal string
// `"input_notify"`. Re-exported here so the bet-with-delay scenario
// doesn't need to import the language module just for one input type.
export const NOTIFY_INPUT: INotify = "input_notify";

// Waits for the contract to close (state and currentContract both null).
// Used at the end of a bet lifecycle after the oracle submits a choice.
export const waitForContractClose = (opts: {
  contractId: ContractId;
  logLabel: string;
  runtime: MarloweRuntimeConfig;
}): ResultAsync<ContractState, unknown> => {
  const { contractId, logLabel, runtime } = opts;
  console.log(`[${logLabel}] waitForContractClose: contractId=${contractId}`);
  return waitPatientlyForResultAsync(
    () => toAsync(marloweRuntimeCli.runGet(contractId, runtime, {}, null, true)),
    (state: ContractState) =>
      state.contractId === contractId &&
      // The settlement branch always reaches close (which nulls both
      // fields), so "closed" is a reliable success predicate.
      state.state === null &&
      state.currentContract === null,
    { timeoutMs: POLL_TIMEOUT_MS, everyMs: POLL_EVERY_MS },
  );
};

// Orchestrates a full bet lifecycle:
//   1. build the `Bet` contract from the wallet addresses + amounts
//   2. init the contract (faucet pays for creation)
//   3. wait for party1's deposit to become applicable, then submit
//   4. wait for party2's deposit to become applicable, then submit
//   5. wait for oracle's choice to become applicable, then submit
//   6. wait for the contract to close
//
// The deposit/choice inputs are built from the `Bet` contract via the
// helpers in {@link Bet}. The waits on `/next` are what prevents the
// runtime from rejecting our next apply-inputs because its chain indexer
// has not yet caught up to our previous submit.
export const run = async (opts: RunOpts): Promise<void> => {
  const { runtime, amount, oracleFee, party1, party2, oracle, faucet, winningChoice } = opts;

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
    timeout,
  );
  const result = await initBetContract({
      contract,
      party1,
      faucet,
      runtime,
    })
    .andThen(contractId =>
      applyInput({
        contractId,
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
      ),
    )
    .andThen(contractId =>
      applyInput({
        contractId,
        input: Bet.mkSecondDepositInput(contract),
        party: party2,
        logLabel: 'after-party2-deposit',
        runtime,
      })
      .andThen(contractIdAfter =>
        waitForNext({
          contractId: contractIdAfter,
          party: oracle,
          kind: 'choice',
          logLabel: 'after-party2-deposit',
          runtime,
        }).map(() => contractIdAfter),
      ),
    )
    .andThen(contractId =>
      applyInput({
        contractId,
        input: Bet.mkOracleChoiceInput(contract, winningChoice),
        party: oracle,
        logLabel: 'after-oracle-choice',
        runtime,
      })
      .andThen(contractIdAfter =>
        waitForContractClose({
          contractId: contractIdAfter,
          logLabel: 'after-oracle-choice',
          runtime,
        }),
      ),
    );

  result.match(
    (finalState: ContractState) => {
      console.log("Final contract state:", finalState);
    },
    (error: unknown) => {
      if (typeof error === "object" && error !== null && "stderr" in error) {
        console.error((error as { stderr: unknown }).stderr);
      } else {
        console.error(error);
      }
      throw new Error(`Bet run failed: ${String(error)}`);
    },
  );
};
