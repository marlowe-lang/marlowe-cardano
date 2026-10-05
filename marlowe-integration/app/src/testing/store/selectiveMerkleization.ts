import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import { assert, unwrapOrPanicWith } from '@konduit/konduit-consumer/neverthrow';
import type { Json } from "@konduit/codec/json";
import { stringify as jsonStringify } from '@konduit/codec/json';
import type { ContractSourceId, PostContractSourceResponse } from '@marlowe-lang/runtime/client';
import type { Case, Contract, NormalCase } from '@marlowe-lang/language/v1';
import type { MarloweRuntimeConfig } from '../../marloweRuntimeCli.js';

type RunOpts = {
  runtime: MarloweRuntimeConfig;
};

const PARTY1_ADDR = 'addr_test1vq2apcdfv7y9tm6gc2090th8747vtqmhu6vemjh0uzzu7pgcy9gsn';
const PARTY2_ADDR = 'addr_test1vq2apcdfv7y9tm6gc2090th8747vtqmhu6vemjh0uzzu7pgcy9gsn';
const ORACLE_ROLE = 'oracle-role';
const CHOICE_NAME = 'team-1-vs-team-2';
const TIMEOUT = 1_700_000_000_000n;
const NO_WINNERS = 0n;
const TEAM_1_WINS = 1n;
const TEAM_2_WINS = 2n;
const AMOUNT = 1_000_000n;

const LOVELACE = { currency_symbol: '', token_name: '' };

const preservedAction = {
  choose_between: [{ from: NO_WINNERS, to: TEAM_2_WINS }],
  for_choice: {
    choice_name: CHOICE_NAME,
    choice_owner: { role_token: ORACLE_ROLE },
  },
};

const betContract = {
  when: [
    {
      case: {
        party: { address: PARTY1_ADDR },
        deposits: AMOUNT,
        of_token: LOVELACE,
        into_account: { address: PARTY1_ADDR },
      },
      then: {
        when: [
          {
            case: {
              party: { address: PARTY2_ADDR },
              deposits: AMOUNT,
              of_token: LOVELACE,
              into_account: { address: PARTY2_ADDR },
            },
            then: {
              when: [
                {
                  case: preservedAction,
                  then: {
                    if: {
                      value: {
                        value_of_choice: {
                          choice_name: CHOICE_NAME,
                          choice_owner: { role_token: ORACLE_ROLE },
                        },
                      },
                      equal_to: TEAM_1_WINS,
                    },
                    then: {
                      pay: AMOUNT,
                      token: LOVELACE,
                      from_account: { address: PARTY2_ADDR },
                      to: { party: { address: PARTY1_ADDR } },
                      then: 'close',
                    },
                    else: {
                      if: {
                        value: {
                          value_of_choice: {
                            choice_name: CHOICE_NAME,
                            choice_owner: { role_token: ORACLE_ROLE },
                          },
                        },
                        equal_to: TEAM_2_WINS,
                      },
                      then: {
                        pay: AMOUNT,
                        token: LOVELACE,
                        from_account: { address: PARTY1_ADDR },
                        to: { party: { address: PARTY2_ADDR } },
                        then: 'close',
                      },
                      else: 'close',
                    },
                  },
                },
              ],
              timeout: TIMEOUT,
              timeout_continuation: 'close',
            },
          },
        ],
        timeout: TIMEOUT,
        timeout_continuation: 'close',
      },
    },
  ],
  timeout: TIMEOUT,
  timeout_continuation: 'close',
};

// Traverses a contract depth-first to find the first non-merkleized case. Returns null if all cases are merkleized.
export const findFirstNonMerkleizedCase = (contract: Contract, getContinuation: ((contractHash: ContractSourceId) => Contract)): NormalCase | null => {
  if (contract === 'close') {
    return null;
  }
  if ('pay' in contract) {
    return findFirstNonMerkleizedCase(contract.then, getContinuation);
  }
  if ('if' in contract) {
    const thenCase = findFirstNonMerkleizedCase(contract.then, getContinuation);
    if (thenCase) {
      return thenCase;
    }
    return findFirstNonMerkleizedCase(contract.else, getContinuation);
  }
  if ('when' in contract) {
    for (const c of contract.when) {
      if ('case' in c && 'then' in c) {
        return c;
      }
      if ('merkleized_then' in c) {
        const continuation = getContinuation(c.merkleized_then as ContractSourceId);
        return findFirstNonMerkleizedCase(continuation, getContinuation);
      }
    }
    return findFirstNonMerkleizedCase(contract.timeout_continuation, getContinuation);
  }
  if ('let' in contract) {
    return findFirstNonMerkleizedCase(contract.then, getContinuation);
  }
  assert('assert' in contract, 'Contract must be one of the known types');
  return findFirstNonMerkleizedCase(contract.then, getContinuation);
}


export const run = async (opts: RunOpts): Promise<void> => {
  const { runtime } = opts;
  const bundle = [
    { label: 'main', type: 'contract', value: betContract },
  ];

  const selectivelyMerkleized = unwrapOrPanicWith(
    marloweRuntimeCli.runUploadContractSource(
        bundle,
        'main',
        runtime,
        { preserveActions: [preservedAction] },
        null,
        true
    ),
    (err): string => `Failed to upload bet bundle with preserved actions: ${jsonStringify(err as Json)}`,
  );

  const fullyMerkleized = unwrapOrPanicWith(
    marloweRuntimeCli.runUploadContractSource(
        bundle,
        'main',
        runtime,
        { preserveActions: [] },
        null,
        true
    ),
    (err): string => `Failed to upload bet bundle with preserved actions: ${jsonStringify(err as Json)}`,
  );

  const getContract = (contractHash: ContractSourceId): Contract => {
    const json = unwrapOrPanicWith(
      marloweRuntimeCli.runGetContractSource(
        contractHash,
        runtime,
        { expand: false },
        null,
        true,
      ),
      (err): string => `Failed to GET continuation contract source: ${jsonStringify(err as Json)}`,
    );
    return json as Contract;
  }

  const nonMerkleizedRoot = getContract(selectivelyMerkleized.contractSourceId);
  const firstNonMerkleizedCase = findFirstNonMerkleizedCase(nonMerkleizedRoot, getContract) as Json;
  if (!firstNonMerkleizedCase) {
    throw new Error(`Expected to find at least one non-merkleized case in the selectively merkleized contract.`);
  }
  console.log(`Found first non-merkleized case: ${jsonStringify(firstNonMerkleizedCase, undefined, 2)}`);

  const fullyMerkleizedRoot = getContract(fullyMerkleized.contractSourceId);
  const firstNonMerkleizedCaseInFullyMerkleized = findFirstNonMerkleizedCase(fullyMerkleizedRoot, getContract) as Json;
  if (firstNonMerkleizedCaseInFullyMerkleized) {
    throw new Error(`Expected to find no non-merkleized cases in the fully merkleized contract, but found: ${jsonStringify(firstNonMerkleizedCaseInFullyMerkleized, undefined, 2)}`);
  }
};

