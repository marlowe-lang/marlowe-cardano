import { AddressBech32 } from '@konduit/konduit-consumer/cardano';
import type {
  Contract,
  IChoice,
  IDeposit,
  NormalInput,
  Observation,
  Value,
} from '@marlowe-lang/language/v1';
import * as lang from '@marlowe-lang/language/v1';
import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import * as cardanoCli from '../../cardanoCli.js';
import { unwrapOk } from '../neverthrow.js';
import { POSIXMilliseconds } from '@konduit/konduit-consumer/time/absolute';
import { Milliseconds, Seconds, Minutes, Hours } from '@konduit/konduit-consumer/time/duration';
import { toAsync } from '@konduit/konduit-consumer/neverthrow';
import type { ResultAsync } from 'neverthrow';
import { ResultAsync as ResultAsyncCtor } from 'neverthrow';
import { waitPatientlyForResultAsync } from '../../neverthrow.js';
import type { Wallet } from '../../cardano.js';
import {
  ApplyInputsResponse,
  ContractId,
  ContractState,
  Next,
  PostCreateContractResponse,
} from '@marlowe-lang/runtime/client';

function mkDepositContract(opts: {
  partyAddr: AddressBech32;
  amount: bigint;
  timeout: POSIXMilliseconds;
  continuation: Contract;
}): Contract {
  const { partyAddr, amount, timeout, continuation } = opts;
  return {
    when: [
      {
        case: {
          party: { address: partyAddr },
          deposits: amount,
          of_token: lang.lovelace,
          into_account: { address: partyAddr },
        },
        then: continuation,
      },
    ],
    timeout: BigInt(timeout),
    timeout_continuation: "close",
  };
}

function mkChoiceContract(opts: {
  partyAddr: AddressBech32;
  choiceName: string;
  bounds: { from: bigint; to: bigint }[];
  timeout: POSIXMilliseconds;
  continuation: Contract;
}): Contract {
  const { partyAddr, choiceName, bounds, timeout, continuation } = opts;
  return {
    when: [
      {
        case: {
          choose_between: bounds,
          for_choice: {
            choice_name: choiceName,
            choice_owner: { address: partyAddr },
          },
        },
        then: continuation,
      },
    ],
    timeout: BigInt(timeout),
    timeout_continuation: "close",
  };
}

// Builder: the settlement contract. Branches on the oracle's choice value:
//   1 -> pay `amount` from account(party2Addr) to party(party1Addr), then
//        close (party1 also recovers their own deposit via close's refund).
//   2 -> symmetric.
//   otherwise (0 / no winners, or the oracle timed out, though timeout would
//   have already reduced to close) -> close, which refunds both accounts.
function mkSettlementContract(opts: {
  party1Addr: AddressBech32;
  party2Addr: AddressBech32;
  amount: bigint;
  choiceName: string;
  party1WinsChoice: bigint;
  party2WinsChoice: bigint;
  oracleAddr: AddressBech32;
}): Contract {
  const { party1Addr, party2Addr, amount, choiceName, party1WinsChoice, party2WinsChoice, oracleAddr } = opts;

  const choiceValue: Value = {
    value_of_choice: {
      choice_name: choiceName,
      choice_owner: { address: oracleAddr },
    },
  };
  const team1Wins: Observation = { value: choiceValue, equal_to: party1WinsChoice };
  const team2Wins: Observation = { value: choiceValue, equal_to: party2WinsChoice };

  const payLoserStakeToWinner = (winner: AddressBech32, loser: AddressBech32): Contract => ({
    pay: amount,
    token: lang.lovelace,
    from_account: { address: loser },
    to: { party: { address: winner } },
    then: "close",
  });

  return {
    if: team1Wins,
    then: payLoserStakeToWinner(party1Addr, party2Addr),
    else: {
      if: team2Wins,
      then: payLoserStakeToWinner(party2Addr, party1Addr),
      else: "close",
    },
  };
}

// Oracle choice encoding for the bet:
//   0 = no winners (close refunds each party from their own account)
//   1 = team 1 won (party2's stake is paid to party1)
//   2 = team 2 won (party1's stake is paid to party2)
const CHOICE_NAME = "team-1-vs-team-2";
const NO_WINNERS = 0n;
const TEAM_1_WINS = 1n;
const TEAM_2_WINS = 2n;

// Six hours expressed properly. `Milliseconds.fromDigits(6, 0, 0, 0, 0, 0)` does
// NOT mean 6 hours — it constructs a `Milliseconds` whose first positional
// digit is `6` and the rest are sub-millisecond zeroed, so the resulting
// `Milliseconds` is `6n`. Use the explicit `Hours` + unit-conversion chain
// so the contract's `When` cases get a real 6-hour timeout. The contract
// being 10 minutes wide (the previous bug) made every subsequent /next
// call fall after `timeout` and trip the validator.
export const CONTRACT_TIMEOUT = Milliseconds.fromSeconds(
  Seconds.fromMinutes(Minutes.fromHours(Hours.fromDigits(6))),
);

// Builds a two-party betting contract settled by an oracle.
//
// Each party deposits `amount` of lovelace into their own account (sequentially,
// party1 first, then party2). Once both deposits are in, the oracle submits
// a single numeric choice in [0,2] via a Choice action:
//   0 -> close (both parties refunded from their own accounts)
//   1 -> party2's stake is paid to party1, then close (party1's own deposit
//        is also refunded from their account by close)
//   2 -> symmetric
//
// `timeout` is the absolute POSIX-ms deadline applied to every stage. Each
// stage's `timeout_continuation` is `close`, so any participant failing to act
// in time simply causes the contract to close and refund whatever has already
// been deposited.
export function mkContract(
  amount: bigint,
  party1Addr: AddressBech32,
  party2Addr: AddressBech32,
  oracleAddr: AddressBech32,
  timeout: POSIXMilliseconds
): Contract {
  return mkDepositContract({
    partyAddr: party1Addr,
    amount,
    timeout,
    continuation: mkDepositContract({
      partyAddr: party2Addr,
      amount,
      timeout,
      continuation: mkChoiceContract({
        partyAddr: oracleAddr,
        choiceName: CHOICE_NAME,
        bounds: [{ from: NO_WINNERS, to: TEAM_2_WINS }],
        timeout,
        continuation: mkSettlementContract({
          party1Addr,
          party2Addr,
          amount,
          choiceName: CHOICE_NAME,
          party1WinsChoice: TEAM_1_WINS,
          party2WinsChoice: TEAM_2_WINS,
          oracleAddr,
        })
      })
    })
  });
}

export type WinningChoice = "no-winners" | "party1-wins" | "party2-wins";
export namespace WinningChoice {
  export const toChoiceValue = (choice: WinningChoice): bigint => {
    switch (choice) {
      case "no-winners":
        return NO_WINNERS;
      case "party1-wins":
        return TEAM_1_WINS;
      case "party2-wins":
        return TEAM_2_WINS;
    }
  }
}

export type RunOpts = {
  amount: bigint;
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

export const mkContractTimeout = (): POSIXMilliseconds => {
  const result = unwrapOk(
    POSIXMilliseconds.addMilliseconds(POSIXMilliseconds.now(), CONTRACT_TIMEOUT),
  );
  // DEBUG: log what we send so we can compare with what the runtime stores.
  console.log(
    `[mkContractTimeout] test.now=${POSIXMilliseconds.now().toString()} ms ` +
    `+ ${CONTRACT_TIMEOUT.toString()} ms (6h) = ${result.toString()} ms ` +
    `(${new Date(Number(result)).toISOString()})`
  );
  return result;
};

// Builds the IDeposit input a party uses to fund their own account.
const mkDepositInput = (partyAddr: AddressBech32, amount: bigint): IDeposit => ({
  input_from_party: { address: partyAddr },
  that_deposits: amount,
  of_token: lang.lovelace,
  into_account: { address: partyAddr },
});

// Builds the IChoice input the oracle uses to settle the bet.
const mkChoiceInput = (oracleAddr: AddressBech32, chosenNum: bigint): IChoice => ({
  for_choice_id: {
    choice_name: CHOICE_NAME,
    choice_owner: { address: oracleAddr },
  },
  input_that_chooses_num: chosenNum,
});

// Helper: is the runtime reporting that `party` can either deposit or
// choose next? Used to gate each step on the runtime's chain indexer
// catching up to our previous submit.
const addressOf = (p: { address: AddressBech32 } | { role_token: string }): AddressBech32 | null =>
  'address' in p ? p.address : null;

const isApplicableFor = (next: Next, party: Wallet, kind: 'deposit' | 'choice'): boolean => {
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
const getCurrentWhenTimeoutMs = async (contractId: ContractId): Promise<bigint | null> => {
  const result = await toAsync(marloweRuntimeCli.runGet(contractId, {}, null, true));
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
  kind: 'deposit' | 'choice',
): Promise<void> => {
  const stateResult = await toAsync(
    marloweRuntimeCli.runGet(contractId, {}, null, true),
  );
  const nextResult = await toAsync(
    marloweRuntimeCli.runNext(contractId, [party.addr], {}, null, true),
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
  party: Wallet;
  kind: 'deposit' | 'choice';
  logLabel: string;
}): ResultAsync<Next, unknown> => {
  const runNextWithDerivedRange = async (): Promise<Next> => {
    const timeoutMs = await getCurrentWhenTimeoutMs(opts.contractId);
    if (timeoutMs === null) {
      // Contract has no active When (e.g. closed) or runtime unreachable.
      // Fall back to the default wall-clock range so we still poll.
      const fallback = await toAsync(
        marloweRuntimeCli.runNext(opts.contractId, [opts.party.addr], {}, null, true),
      );
      if (fallback.isErr()) throw fallback.error;
      return fallback.value;
    }
    const { fromMs, toMs } = buildValidityRangeFromTimeout(timeoutMs);
    const derived = await toAsync(
      marloweRuntimeCli.runNext(
        opts.contractId,
        [opts.party.addr],
        { validityStart: toIsoUtc(fromMs), validityEnd: toIsoUtc(toMs) },
        null,
        true,
      ),
    );
    if (derived.isErr()) throw derived.error;
    return derived.value;
  };
  let attempt = 0;
  return waitPatientlyForResultAsync(
    () => ResultAsyncCtor.fromPromise(runNextWithDerivedRange(), (e: unknown) => e),
    (next) => {
      const ready = isApplicableFor(next, opts.party, opts.kind);
      // Log a fuller snapshot every 5th attempt to help debug cases where
      // the runtime sees the contract but never reports the expected input.
      if (!ready && attempt % 5 === 0) {
        void logRuntimeSnapshot(opts.logLabel, opts.contractId, opts.party, opts.kind);
      }
      attempt += 1;
      return ready;
    },
    { timeoutMs: POLL_TIMEOUT_MS, everyMs: POLL_EVERY_MS },
  );
};

// Initializes the bet contract via the marlowe-runtime CLI, signs the init
// transaction with the faucet wallet, submits it, and waits until the runtime
// reports party1's deposit as the next applicable input.
const initBetContract = (opts: {
  amount: bigint;
  party1: Wallet;
  party2: Wallet;
  oracle: Wallet;
  faucet: Wallet;
}): ResultAsync<ContractId, unknown> => {
  const { amount, party1, party2, oracle, faucet } = opts;
  const contract = mkContract(
    amount,
    party1.addr,
    party2.addr,
    oracle.addr,
    mkContractTimeout(),
  );
  return toAsync(marloweRuntimeCli.runInit(contract, faucet.addr, {}, null, true))
    .andThen((response: PostCreateContractResponse) =>
      cardanoCli.signTxEnvelope(faucet.skeyFile, response.tx).map(signed => ({
        contractId: response.contractId,
        txEnvelope: signed,
      })),
    )
    .andThen(({ contractId, txEnvelope }) =>
      cardanoCli.submitTxEnvelope(txEnvelope).map(() => contractId),
    )
    .andThen(contractId =>
      // Wait for the runtime to see our init tx and report party1's
      // deposit as the next applicable input.
      waitForNext({ contractId, party: party1, kind: 'deposit', logLabel: 'after-init' })
        .map(() => contractId),
    );
};

// Submits a deposit input on behalf of `party`, then waits for the runtime
// to advance to the next step (`nextParty`'s input is now applicable).
export const applyDeposit = (opts: {
  contractId: ContractId;
  party: Wallet;
  amount: bigint;
  nextParty: Wallet;
  logLabel: string;
}): ResultAsync<ContractId, unknown> => {
  const { contractId, party, amount, nextParty, logLabel } = opts;
  const input: NormalInput = mkDepositInput(party.addr, amount);
  return toAsync(
    marloweRuntimeCli.runApplyInputs([input], contractId, party.addr, {}, null, true),
  )
    .andThen((response: ApplyInputsResponse) =>
      cardanoCli.signTxEnvelope(party.skeyFile, response.tx).map(signed => ({
        contractId: response.contractId,
        txEnvelope: signed,
      })),
    )
    .andThen(({ txEnvelope }) => cardanoCli.submitTxEnvelope(txEnvelope).map(() => contractId))
    .andThen(contractIdAfter =>
      // Wait for the runtime to catch up and report `nextParty`'s input as
      // applicable before returning.
      waitForNext({ contractId: contractIdAfter, party: nextParty, kind: 'deposit', logLabel })
        .map(() => contractIdAfter),
    );
};

// Submits the oracle's choice and waits for the contract to close
// (state and currentContract both null).
export const applyChoice = (opts: {
  contractId: ContractId;
  party: Wallet;
  choiceValue: bigint;
}): ResultAsync<ContractState, unknown> => {
  const { contractId, party, choiceValue } = opts;
  const input: NormalInput = mkChoiceInput(party.addr, choiceValue);
  return toAsync(
    marloweRuntimeCli.runApplyInputs([input], contractId, party.addr, {}, null, true),
  )
    .andThen((response: ApplyInputsResponse) =>
      cardanoCli.signTxEnvelope(party.skeyFile, response.tx).map(signed => ({
        contractId: response.contractId,
        txEnvelope: signed,
      })),
    )
    .andThen(({ txEnvelope }) => cardanoCli.submitTxEnvelope(txEnvelope).map(() => contractId))
    .andThen(contractIdAfter =>
      waitPatientlyForResultAsync(
        () => toAsync(marloweRuntimeCli.runGet(contractIdAfter, {}, null, true)),
        (state: ContractState) =>
          state.contractId === contractIdAfter &&
          // The settlement branch always reaches close (which nulls both
          // fields), so "closed" is a reliable success predicate.
          state.state === null &&
          state.currentContract === null,
        { timeoutMs: POLL_TIMEOUT_MS, everyMs: POLL_EVERY_MS },
      ),
    );
};

// Orchestrates a full bet lifecycle:
//   1. init the contract (faucet pays for creation)
//   2. wait for party1's deposit to become applicable, then submit
//   3. wait for party2's deposit to become applicable, then submit
//   4. wait for oracle's choice to become applicable, then submit
//   5. wait for the contract to close
//
// The waits on `/next` are what prevents the runtime from rejecting our
// next apply-inputs because its chain indexer has not yet caught up to our
// previous submit.
export const run = async (opts: RunOpts): Promise<void> => {
  const { amount, party1, party2, oracle, faucet, winningChoice } = opts;
  const result = await initBetContract({
      amount,
      party1,
      party2,
      oracle,
      faucet,
    })
    .andThen(contractId =>
      applyDeposit({
        contractId,
        party: party1,
        amount,
        nextParty: party2,
        logLabel: 'after-party1-deposit',
      }).map(() => contractId),
    )
    .andThen(contractId =>
      applyDeposit({
        contractId,
        party: party2,
        amount,
        nextParty: oracle,
        logLabel: 'after-party2-deposit',
      }).map(() => contractId),
    )
    .andThen(contractId =>
      applyChoice({
        contractId,
        party: oracle,
        choiceValue: WinningChoice.toChoiceValue(winningChoice),
      }),
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
