/**
 * This module exposes the semantic functions of the Marlowe language. They can be used
 * to simulate/test a contract.
 ```
 import * as G from "@marlowe.io/language-core-v1/guards";
 import { Timeout } from "@marlowe.io/language-core-v1";
 import { playTrace } from "@marlowe.io/language-core-v1/semantics";
 import { role, lovelace } from "@marlowe.io/language-core-v1/playground-v1";
 import { escrow } from "@marlowe.io/language-examples";

 const contract = escrow({
   price: 100,
   depositTimeout: Timeout.fromDate(new Date("2050-06-01T00:00:00Z")),
   disputeTimeout: Timeout.fromDate(new Date("2050-06-02T00:00:00Z")),
   answerTimeout: Timeout.fromDate(new Date("2050-06-03T00:00:00Z")),
   arbitrageTimeout: Timeout.fromDate(new Date("2050-06-04T00:00:00Z")),
 });

 const txs = [
   {
     tx_interval: {
       from: Timeout.fromDate(new Date("2050-05-01T00:00:00Z")),
       to: Timeout.fromDate(new Date("2050-05-02T00:00:00Z")),
     },
     tx_inputs: [
       {
         input_from_party: role("Buyer"),
         that_deposits: 100n,
         of_token: lovelace,
         into_account: role("Seller"),
       },
       {
         for_choice_id: {
           choice_name: "Everything is alright",
           choice_owner: role("Buyer"),
         },
         input_that_chooses_num: 0n,
       },
     ],
   },
 ];

 const txOut = playTrace(
   Timeout.fromDate(new Date("2000-06-01T00:00:00Z")),
   contract,
   txs
 );

 if (G.TransactionSuccess.is(txOut)) {
   const { state, contract, warnings, payments} = txOut;
   console.log(state, contract, warnings, payments);
 }
 ```
  * @packageDocumentation
  */
import * as AssocMap from "./assoc-map.js";
import { deepEqual } from "./deep-equal.js";
import type { Action } from "./v1/actions.js";
import { choiceIdCmp, inBounds } from "./v1/choices.js";
import type { Case, Contract } from "./v1/contract.js";
import type { Environment, TimeInterval } from "./v1/environment.js";
import type { IChoice, IDeposit, Input, InputContent } from "./v1/inputs.js";
import type { Party } from "./v1/participants.js";
import type { AccountId, Payee } from "./v1/payee.js";
import type { Accounts, MarloweState } from "./v1/state.js";
import { accountsCmp } from "./v1/state.js";
import type { Token } from "./v1/token.js";
import type {
  AmbiguousTimeIntervalError,
  ApplyNoMatchError,
  AssertionFailed,
  HashMismatchError,
  IntervalError,
  NonPositiveDeposit,
  NonPositivePay,
  PartialPay,
  Payment,
  Shadowing,
  SingleInputTx,
  Transaction,
  TransactionError,
  TransactionOutput,
  TransactionWarning,
} from "./v1/transaction.js";
import type { Observation, Value, AvailableMoney, ChoiceValue, UseValue, NegValue, AddValue, SubValue, MulValue, DivValue, Cond } from "./v1/value-and-observation.js";
import type { POSIXTime } from "./time.js";
import { min, max } from "./bigint.js";

export type {
  Payment,
  Transaction,
  SingleInputTx,
  NonPositiveDeposit,
  NonPositivePay,
  PartialPay,
  Shadowing,
  AssertionFailed,
  TransactionWarning,
  InvalidInterval,
  IntervalInPast,
  IntervalError,
  AmbiguousTimeIntervalError,
  ApplyNoMatchError,
  UselessTransaction,
  HashMismatchError,
  TEIntervalError,
  TransactionError,
  TransactionSuccess,
  TransactionOutput,
} from "./v1/transaction.js";
export { inBounds };
/**
 * The function moneyInAccount returns the number of tokens a particular AccountId has in their account.
 * @hidden
 */
function moneyInAccount(accountId: AccountId, token: Token, accounts: Accounts): bigint {
  return AssocMap.findWithDefault(accountsCmp, 0n, [accountId, token], accounts);
}

/**
 * The function updateMoneyInAccount sets the amount of tokens a particular AccountId has in their account,
 * overriding any previous value. Marlowe re- quires all accounts to have positive balances, so if the
 * function is called with a negative value or zero the entry is removed from the accounts.
 * @hidden
 */
function updateMoneyInAccount(accountId: AccountId, token: Token, amount: bigint, accounts: Accounts): Accounts {
  if (amount <= 0n) {
    return AssocMap.remove(accountsCmp, [accountId, token], accounts);
  } else {
    return AssocMap.insert(accountsCmp, [accountId, token], amount, accounts);
  }
}

/**
 * The function addMoneyToAccount increases the amount of tokens in an ac- count identified by a
 * particular AccountId. To ensure that the value always increases we check that money > 0.
 * @hidden
 */
function addMoneyToAccount(accountId: AccountId, token: Token, amount: bigint, accounts: Accounts): Accounts {
  if (amount <= 0n) {
    return accounts;
  } else {
    const balance = moneyInAccount(accountId, token, accounts);
    return updateMoneyInAccount(accountId, token, balance + amount, accounts);
  }
}

/**
 *
 * @param env
 * @param state
 * @param value
 * @returns
 * @category Evaluation
 */
export function evalValue(env: Environment, state: MarloweState, value: Value): bigint {
  if (typeof value === "bigint") {
    return value;
  }
  if (isAvailableMoney(value)) {
    return moneyInAccount(value.in_account, value.amount_of_token, state.accounts);
  }
  if (isChoiceValue(value)) {
    return AssocMap.findWithDefault(choiceIdCmp, 0n, value.value_of_choice, state.choices);
  }
  if (isUseValue(value)) {
    return AssocMap.findWithDefault(AssocMap.strCmp, 0n, value.use_value, state.boundValues);
  }
  if (isNegValue(value)) {
    return -evalValue(env, state, value.negate);
  }
  if (isAddValue(value)) {
    return evalValue(env, state, value.add) + evalValue(env, state, value.and);
  }
  if (isSubValue(value)) {
    return evalValue(env, state, value.value) - evalValue(env, state, value.minus);
  }
  if (isMulValue(value)) {
    return evalValue(env, state, value.multiply) * evalValue(env, state, value.times);
  }
  if (isDivValue(value)) {
    const n = evalValue(env, state, value.divide);
    const d = evalValue(env, state, value.by);
    if (d === 0n) {
      return 0n;
    }
    return n / d;
  }
  if (value === "time_interval_start") {
    return env.timeInterval.from;
  }
  if (value === "time_interval_end") {
    return env.timeInterval.to;
  }
  if (isCond(value)) {
    return evalObservation(env, state, value.if) ? evalValue(env, state, value.then) : evalValue(env, state, value.else);
  }
  return 0n;
}

function isAvailableMoney(v: Value): v is AvailableMoney {
  return typeof v === "object" && v !== null && "amount_of_token" in v;
}
function isChoiceValue(v: Value): v is ChoiceValue {
  return typeof v === "object" && v !== null && "value_of_choice" in v;
}
function isUseValue(v: Value): v is UseValue {
  return typeof v === "object" && v !== null && "use_value" in v;
}
function isNegValue(v: Value): v is NegValue {
  return typeof v === "object" && v !== null && "negate" in v;
}
function isAddValue(v: Value): v is AddValue {
  return typeof v === "object" && v !== null && "add" in v && "and" in v;
}
function isSubValue(v: Value): v is SubValue {
  return typeof v === "object" && v !== null && "value" in v && "minus" in v;
}
function isMulValue(v: Value): v is MulValue {
  return typeof v === "object" && v !== null && "multiply" in v && "times" in v;
}
function isDivValue(v: Value): v is DivValue {
  return typeof v === "object" && v !== null && "divide" in v && "by" in v;
}
function isCond(v: Value): v is Cond {
  return typeof v === "object" && v !== null && "if" in v && "then" in v && "else" in v;
}

/**
 *
 * @param env
 * @param state
 * @param obs
 * @returns
 * @category Evaluation
 */
export function evalObservation(env: Environment, state: MarloweState, obs: Observation): boolean {
  if (typeof obs === "boolean") {
    return obs;
  }
  if (isAndObs(obs)) {
    return evalObservation(env, state, obs.both) && evalObservation(env, state, obs.and);
  }
  if (isOrObs(obs)) {
    return evalObservation(env, state, obs.either) || evalObservation(env, state, obs.or);
  }
  if (isNotObs(obs)) {
    return !evalObservation(env, state, obs.not);
  }
  if (isChoseSomething(obs)) {
    return AssocMap.member(choiceIdCmp, obs.chose_something_for, state.choices);
  }
  if (isValueGE(obs)) {
    return evalValue(env, state, obs.value) >= evalValue(env, state, obs.ge_than);
  }
  if (isValueGT(obs)) {
    return evalValue(env, state, obs.value) > evalValue(env, state, obs.gt);
  }
  if (isValueLT(obs)) {
    return evalValue(env, state, obs.value) < evalValue(env, state, obs.lt);
  }
  if (isValueLE(obs)) {
    return evalValue(env, state, obs.value) <= evalValue(env, state, obs.le_than);
  }
  if (isValueEQ(obs)) {
    return evalValue(env, state, obs.value) === evalValue(env, state, obs.equal_to);
  }
  return false;
}

type AndObs = { both: Observation; and: Observation };
type OrObs = { either: Observation; or: Observation };
type NotObs = { not: Observation };
type ChoseSomething = { chose_something_for: { choice_name: string; choice_owner: Party } };
type ValueGE = { value: Value; ge_than: Value };
type ValueGT = { value: Value; gt: Value };
type ValueLT = { value: Value; lt: Value };
type ValueLE = { value: Value; le_than: Value };
type ValueEQ = { value: Value; equal_to: Value };

function isAndObs(v: Observation): v is AndObs {
  return typeof v === "object" && v !== null && "both" in v && "and" in v;
}
function isOrObs(v: Observation): v is OrObs {
  return typeof v === "object" && v !== null && "either" in v && "or" in v;
}
function isNotObs(v: Observation): v is NotObs {
  return typeof v === "object" && v !== null && "not" in v;
}
function isChoseSomething(v: Observation): v is ChoseSomething {
  return typeof v === "object" && v !== null && "chose_something_for" in v;
}
function isValueGE(v: Observation): v is ValueGE {
  return typeof v === "object" && v !== null && "value" in v && "ge_than" in v;
}
function isValueGT(v: Observation): v is ValueGT {
  return typeof v === "object" && v !== null && "value" in v && "gt" in v;
}
function isValueLT(v: Observation): v is ValueLT {
  return typeof v === "object" && v !== null && "value" in v && "lt" in v;
}
function isValueLE(v: Observation): v is ValueLE {
  return typeof v === "object" && v !== null && "value" in v && "le_than" in v;
}
function isValueEQ(v: Observation): v is ValueEQ {
  return typeof v === "object" && v !== null && "value" in v && "equal_to" in v;
}