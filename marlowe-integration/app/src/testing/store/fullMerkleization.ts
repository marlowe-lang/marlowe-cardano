import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import { unwrapOrPanicWith } from '@konduit/konduit-consumer/neverthrow';
import type { Json } from "@konduit/codec/json";
import { stringify as jsonStringify } from '@konduit/codec/json';
import type { PostContractSourceResponse } from '@marlowe-lang/runtime/client';
import {
  Address,
  Bound,
  Case,
  Choice,
  ChoiceId,
  ChoiceValue,
  Close,
  Constant,
  Deposit,
  If,
  lovelace,
  Party,
  Pay,
  PayeeParty,
  ValueEQ,
  When,
} from '@marlowe-lang/language/v1';
import type { Contract } from '@marlowe-lang/language/v1';
import type { MarloweRuntimeConfig } from '../../marloweRuntimeCli.js';

type RunOpts = {
  runtime: MarloweRuntimeConfig;
};

// FIXME: paluh: round-trip the expanded contract through a real `Contract`
// JSON codec once one is exposed by `@marlowe-lang/language` so we can drop
// the manual structural checks below.

// Hard-coded two-party bet contract wrapped under a `main` label.
// The runtime does not validate timeouts/amounts so we can pick arbitrary
// values. We use fixed addresses so the test is deterministic.
const PARTY1_ADDR = 'addr_test1vq2apcdfv7y9tm6gc2090th8747vtqmhu6vemjh0uzzu7pgcy9gsn';
const PARTY2_ADDR = 'addr_test1vq2apcdfv7y9tm6gc2090th8747vtqmhu6vemjh0uzzu7pgcy9gsn';
const ORACLE_ADDR = 'addr_test1vq2apcdfv7y9tm6gc2090th8747vtqmhu6vemjh0uzzu7pgcy9gsn';
const CHOICE_NAME = 'team-1-vs-team-2';
const TIMEOUT = 1_700_000_000_000n;
const NO_WINNERS = 0n;
const TEAM_1_WINS = 1n;
const TEAM_2_WINS = 2n;
const AMOUNT = 1_000_000n;

// This is the expanded plain form of the contract.
// const betContract = {
//   when: [
//     {
//       case: {
//         party: { address: PARTY1_ADDR },
//         deposits: AMOUNT,
//         of_token: LOVELACE,
//         into_account: { address: PARTY1_ADDR },
//       },
//       then: {
//         when: [
//           {
//             case: {
//               party: { address: PARTY2_ADDR },
//               deposits: AMOUNT,
//               of_token: LOVELACE,
//               into_account: { address: PARTY2_ADDR },
//             },
//             then: {
//               when: [
//                 {
//                   case: {
//                     choose_between: [{ from: NO_WINNERS, to: TEAM_2_WINS }],
//                     for_choice: {
//                       choice_name: CHOICE_NAME,
//                       choice_owner: { address: ORACLE_ADDR },
//                     },
//                   },
//                   then: {
//                     if: {
//                       value: {
//                         value_of_choice: {
//                           choice_name: CHOICE_NAME,
//                           choice_owner: { address: ORACLE_ADDR },
//                         },
//                       },
//                       equal_to: TEAM_1_WINS,
//                     },
//                     then: {
//                       pay: AMOUNT,
//                       token: LOVELACE,
//                       from_account: { address: PARTY2_ADDR },
//                       to: { party: { address: PARTY1_ADDR } },
//                       then: 'close',
//                     },
//                     else: {
//                       if: {
//                         value: {
//                           value_of_choice: {
//                             choice_name: CHOICE_NAME,
//                             choice_owner: { address: ORACLE_ADDR },
//                           },
//                         },
//                         equal_to: TEAM_2_WINS,
//                       },
//                       then: {
//                         pay: AMOUNT,
//                         token: LOVELACE,
//                         from_account: { address: PARTY1_ADDR },
//                         to: { party: { address: PARTY2_ADDR } },
//                         then: 'close',
//                       },
//                       else: 'close',
//                     },
//                   },
//                 },
//               ],
//               timeout: TIMEOUT,
//               timeout_continuation: 'close',
//             },
//           },
//         ],
//         timeout: TIMEOUT,
//         timeout_continuation: 'close',
//       },
//     },
//   ],
//   timeout: TIMEOUT,
//   timeout_continuation: 'close',
// };

// New definition using the language package's smart constructors.
// `Close()` / `Party(Address(addr))` / `Bound` / `When` / `Case` / `Deposit` /
// `Choice` / `Pay` / `If` / `ChoiceValue` / `ValueEQ` all share their name
// with the matching `type` and `namespace` triple.

const party1 = Party(Address(PARTY1_ADDR));
const party2 = Party(Address(PARTY2_ADDR));
const oracle = Party(Address(ORACLE_ADDR));
const choiceId = ChoiceId(CHOICE_NAME, oracle);
const choiceValue = ChoiceValue(choiceId);

const betContract: Contract = When(
  [
    Case(
      Deposit(party1, party1, lovelace, AMOUNT),
      When(
        [
          Case(
            Deposit(party2, party2, lovelace, AMOUNT),
            When(
              [
                Case(
                  Choice([Bound(NO_WINNERS, TEAM_2_WINS)], choiceId),
                  If(
                    ValueEQ(choiceValue, Constant(TEAM_1_WINS)),
                    Pay(AMOUNT, lovelace, party2, PayeeParty(party1), Close()),
                    If(
                      ValueEQ(choiceValue, Constant(TEAM_2_WINS)),
                      Pay(AMOUNT, lovelace, party1, PayeeParty(party2), Close()),
                      Close(),
                    ),
                  ),
                ),
              ],
              TIMEOUT,
              Close(),
            ),
          ),
        ],
        TIMEOUT,
        Close(),
      ),
    ),
  ],
  TIMEOUT,
  Close(),
);

// FIXME: paluh: round-trip the expanded contract through a real `Contract`
// JSON codec once one is exposed by `@marlowe-lang/language` so we can drop
// the manual structural checks below.

// Deep, order-insensitive structural equality for JSON-like values. Treats
// `bigint` and `number` as equal when their values match (the runtime's
// JSON serialisation strips the bigint distinction).
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

// Exercises the contract source store with a non-trivial contract. The
// runtime should:
//   1. accept the upload of the `bet` contract wrapped under `main`,
//   2. report a non-empty `intermediateIds` map (the deep continuations
//      get merkleized),
//   3. report a closure that includes every intermediate id plus the main,
//   4. round-trip the expanded (de-merkleized) form back to the original.
export const run = async (opts: RunOpts): Promise<void> => {
  const { runtime } = opts;
  const bundle = [
    { label: 'main', type: 'contract', value: betContract },
  ];

  const uploadResult = marloweRuntimeCli.runUploadContractSource(
    bundle,
    'main',
    runtime,
    {},
    null,
    true,
  );
  const uploaded = unwrapOrPanicWith(
    uploadResult,
    (err): string => `Failed to upload bet bundle: ${jsonStringify(err as Json)}`,
  );

  if (!uploaded.contractSourceId || uploaded.contractSourceId.length !== 64) {
    throw new Error(`Expected a 64-hex-char contractSourceId, got: ${uploaded.contractSourceId}`);
  }
  if (!uploaded.intermediateIds.main) {
    throw new Error(`Expected the uploaded bundle to advertise a 'main' intermediate id`);
  }

  const sourceId = uploaded.contractSourceId;

  // The raw form is stored in merkleized form (nested continuations are
  // replaced by `merkleized_then` references), so we can't round-trip it
  // against the original.
  const rawResult = marloweRuntimeCli.runGetContractSource(
    sourceId,
    runtime,
    { expand: false },
    null,
    true,
  );
  unwrapOrPanicWith(
    rawResult,
    (err): string => `Failed to GET raw contract source: ${jsonStringify(err as Json)}`,
  );

  // GET expanded — the de-merkleized form should equal the original
  // structurally (the runtime alphabetizes object keys, so a string
  // comparison is too strict).
  const expandedResult = marloweRuntimeCli.runGetContractSource(
    sourceId,
    runtime,
    { expand: true },
    null,
    true,
  );
  const expanded = unwrapOrPanicWith(
    expandedResult,
    (err): string => `Failed to GET expanded contract source: ${jsonStringify(err as Json)}`,
  );
  if (!structuralEqual(expanded, betContract)) {
    throw new Error(
      `Expanded source does not structurally match the original bet contract.\n` +
        `Got: ${jsonStringify(expanded)}\nExpected: ${jsonStringify(betContract)}`,
    );
  }

  // Closure: should contain the main source id plus every merkleized
  // continuation the runtime created. With a hand-coded single-label
  // bundle we only get `main` back from the upload, but the closure will
  // contain additional entries produced by the merkleization step.
  const closureResult = marloweRuntimeCli.runGetContractSourceClosure(
    sourceId,
    runtime,
    null,
    true,
  );
  const closure = unwrapOrPanicWith(
    closureResult,
    err => `Failed to GET closure: ${jsonStringify(err)}`,
  );
  if (!closure.includes(sourceId)) {
    throw new Error(
      `Expected closure to contain ${sourceId}, got: ${jsonStringify(closure)}`,
    );
  }
  if (closure.length < 2) {
    throw new Error(
      `Expected the merkleized bet contract to produce a non-trivial closure, got: ${jsonStringify(closure)}`,
    );
  }
};

// Suppress unused import warning; PostContractSourceResponse is re-exported
// by the runtime client and may be useful for downstream consumers.
export type { PostContractSourceResponse };
