import { AddressBech32 } from '@konduit/konduit-consumer/cardano';
import {
  Address,
  Choice,
  ChoiceId,
  ChoiceValue,
  Close,
  Constant,
  Contract,
  Deposit,
  IChoice,
  IDeposit,
  If,
  lovelace,
  Party,
  Pay,
  PayeeParty,
  Timeout,
  Value,
  ValueEQ,
  When,
  WhenAction,
} from '@marlowe-lang/language/v1';
import { unwrapOrPanic } from '@konduit/konduit-consumer/neverthrow';
import type { Tagged } from 'type-fest';
import { stringify } from '@konduit/codec/json';
import { pipe } from '@konduit/codec';
import { err, ok, Result } from 'neverthrow';
import type { JsonCodec, JsonError } from '@konduit/codec/json/codecs';

/** Choice name the oracle must submit to settle the bet. */
const CHOICE_NAME = "team-1-vs-team-2";

/**
 * Oracle choice: no winners.
 * Close refunds each party from their own account.
 */
const NO_WINNERS = 0n;

/**
 * Oracle choice: team 1 won.
 * Party 2's stake is paid to party 1.
 */
const TEAM_1_WINS = 1n;

/**
 * Oracle choice: team 2 won.
 * Party 1's stake is paid to party 2.
 */
const TEAM_2_WINS = 2n;

/**
 * A more human-friendly representation of the oracle's choice.
 */
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

/**
 * Two-party betting contract settled by an oracle.
 *
 * Do not cast `Contract` to `Bet`. Construct it with {@link Bet}.
 */
export type Bet = Tagged<Contract, "Bet">;

/**
 * Builds a two-party bet settled by a single oracle choice.
 *
 * Flow:
 * 1. Party 1 deposits `betAmount + party1OracleFee` into their own account.
 * 2. Party 2 deposits `betAmount + party2OracleFee` into their own account.
 * 3. The oracle submits one choice in `[0, 2]`:
 *    - `0`: close; both parties are refunded from their own accounts.
 *    - `1`: party 2's stake is paid to party 1, then close (party 1's deposit is refunded by close).
 *    - `2`: symmetric to `1`.
 *
 * The oracle fee is split with integer division: party 1 pays `oracleFee / 2`,
 * party 2 pays the remainder. Both fee payments happen before settlement.
 *
 * `posixTimeout` is the absolute POSIX-millisecond deadline on every stage.
 * Each stage's timeout continuation is close, so a missed action refunds whatever
 * has already been deposited.
 *
 * @param betAmount - Stake each party deposits, in lovelace. Does not include the oracle fee.
 * @param oracleFee - Total oracle fee, in lovelace. Split across the two parties.
 * @param party1Addr - Bech32 address of party 1. Deposits first.
 * @param party2Addr - Bech32 address of party 2. Deposits second.
 * @param oracleAddr - Bech32 address of the oracle that submits the choice and receives the fee.
 * @param posixTimeout - Absolute POSIX-millisecond deadline applied to every stage.
 * @returns A `Bet` contract. Party 1 deposit, then party 2 deposit, then oracle choice, then settlement.
 */
export function Bet(
  betAmount: bigint,
  oracleFee: bigint,
  party1_: AddressBech32 | Party,
  party2_: AddressBech32 | Party,
  oracle_: AddressBech32 | Party,
  timeout: Timeout
): Bet {
  let isParty = (party: AddressBech32 | Party): party is Party => typeof party !== "string";
  let mkParty = (addr: AddressBech32 | Party): Party => isParty(addr) ? addr : Party(Address(addr));
  let party1 = mkParty(party1_);
  let party2 = mkParty(party2_);
  let oracle = mkParty(oracle_);

  let party1OracleFee = oracleFee / 2n;
  let party2OracleFee = oracleFee - party1OracleFee;

  const choiceValue = ChoiceValue(ChoiceId(CHOICE_NAME, oracle));

  const payLoserStakeToWinner = (winner: Party, loser: Party): Contract =>
    Pay(
      betAmount,
      lovelace,
      loser,
      PayeeParty(winner),
      Close(),
    );

  return WhenAction(
    Deposit(party1, party1, lovelace, betAmount + party1OracleFee),
    timeout,
    WhenAction(
      Deposit(party2, party2, lovelace, betAmount + party2OracleFee),
      timeout,
      WhenAction(
        Choice([{ from: NO_WINNERS, to: TEAM_2_WINS }], ChoiceId(CHOICE_NAME, oracle)),
        timeout,
        // Oracle fee payment
        Pay(party1OracleFee, lovelace, party1, PayeeParty(oracle),
          Pay(party2OracleFee, lovelace, party2, PayeeParty(oracle),
            // Settlement based on oracle choice
            If(
              ValueEQ(choiceValue, Constant(TEAM_1_WINS)),
              payLoserStakeToWinner(party1, party2),
              If(
                ValueEQ(choiceValue, Constant(TEAM_2_WINS)),
                payLoserStakeToWinner(party2, party1),
                // Tie
                Close(),
              ),
            )
          )
        )
      )
    )
  ) as Bet;
}

export namespace Bet {
  const id = <T>(x: T): T => x;
  export const jsonCodec: JsonCodec<Bet> = pipe(
    Contract.jsonCodec,
    { deserialise: (contract: Contract): Result<Bet, JsonError> => When.matchDeposit(contract)
      .andThen(([party1Deposit, then1, timeout, _]) => When.matchDeposit(then1)
        .andThen(([party2Deposit, then2, _t1, _tc1]) => When.matchChoice(then2)
          .andThen(([oracleChoice, then3, _t2, _tc2]) => Contract.tryMatch(then3, {pay: id})
            .andThen((party1FeePayment) => Value.tryMatch(party1FeePayment.pay, {constant: id})
              .andThen((fee1) => Contract.tryMatch(party1FeePayment.then, {pay: id})
                .andThen((party2FeePayment) => Value.tryMatch(party2FeePayment.pay, {constant: id})
                  .andThen((fee2) => Contract.tryMatch(party2FeePayment.then, {if: id})
                    .andThen((if_) => Contract.tryMatch(if_.then, {pay: id})
                      .andThen((party1WinsPayment) => Value.tryMatch(party1WinsPayment.pay, {constant: id})
                        .map((amount) => {
                          const party1 = party1Deposit.party;
                          const party2 = party2Deposit.party;
                          const oracle = oracleChoice.for_choice.choice_owner;
                          return { party1, party2, oracle, fee1, fee2, amount, timeout };
                        })
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ).andThen(({ party1, party2, oracle, fee1, fee2, amount, timeout }) => {
        const reconstructedBet = Bet(
          amount,
          fee1 + fee2,
          party1,
          party2,
          oracle,
          timeout
        );
        if (!Contract.areEqual(contract, reconstructedBet)) {
          return err(`Contract is not a valid Bet: ${stringify(contract)}`);
        }
        return ok(reconstructedBet);
      }),
      serialise: (contract: Bet): Contract => contract
    }
  );

  export const extractOracleChoice = (contract: Bet): Choice =>
    // Pattern match of the known structure of the Bet contract to extract the oracle's Choice action.
    unwrapOrPanic(
      When.matchDeposit(contract)
        .andThen(([, then1]) => When.matchDeposit(then1))
        .andThen(([, then2]) => When.matchChoice(then2))
        .map(([choice, _]) => choice),
      "Failed to extract oracle choice from Bet contract"
    )

  export const mkFirstDepositInput = (contract: Bet): IDeposit =>
    unwrapOrPanic(
      When.matchDeposit(contract)
        .andThen(([deposit, _]) => Value.tryMatch(deposit.deposits, { constant: (c): [Deposit, bigint] => [deposit, c] }))
        .map(([deposit, amount]) => IDeposit(
          deposit.party,
          amount,
          deposit.of_token,
          deposit.into_account
        )),
      "Failed to extract first deposit from Bet contract"
    );

  export const mkSecondDepositInput = (contract: Bet): IDeposit =>
    unwrapOrPanic(
      When.matchDeposit(contract)
        .andThen(([, then1]) => When.matchDeposit(then1))
        .andThen(([deposit, _]) => Value.tryMatch(deposit.deposits, { constant: (c): [Deposit, bigint] => [deposit, c] }))
        .map(([deposit, amount]) => IDeposit(
          deposit.party,
          amount,
          deposit.of_token,
          deposit.into_account
        )),
      "Failed to extract second deposit from Bet contract"
    );

  export const mkOracleChoiceInput = (contract: Bet, winningChoice: WinningChoice): IChoice => {
    const oracleChoice = extractOracleChoice(contract);
    return IChoice(
      oracleChoice.for_choice,
      WinningChoice.toChoiceValue(winningChoice)
    );
  }
}

