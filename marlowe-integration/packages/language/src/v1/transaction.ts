import {
  altJsonCodecs,
  arrayOf,
  constant as jsonConstant,
  json2BigIntCodec,
  json2StringCodec,
  objectOf,
  type JsonCodec,
} from "@konduit/codec/json/codecs";
import { fromCodecThunkFn } from "@konduit/codec";
import { err, ok } from "neverthrow";
import type { Result } from "neverthrow";
import { arrayAreEqualWith } from "../assoc-map.js";
import { Contract } from "./contract.js";
import { TimeInterval } from "./environment.js";
import { Input } from "./inputs.js";
import { Party } from "./participants.js";
import { AccountId, Payee } from "./payee.js";
import { MarloweState } from "./state.js";
import { Token } from "./token.js";

export type Payment = {
  payment_from: AccountId;
  to: Payee;
  amount: bigint;
  token: Token;
};
export function Payment(payment_from: AccountId, to: Payee, amount: bigint, token: Token): Payment {
  return { payment_from, to, amount, token };
}
export namespace Payment {
  export const jsonCodec: JsonCodec<Payment> = objectOf({
    payment_from: AccountId.jsonCodec,
    to: Payee.jsonCodec,
    amount: json2BigIntCodec,
    token: Token.jsonCodec,
  });
  export const areEqual = (a: Payment, b: Payment): boolean =>
    AccountId.areEqual(a.payment_from, b.payment_from) &&
    Payee.areEqual(a.to, b.to) &&
    a.amount === b.amount &&
    Token.areEqual(a.token, b.token);
}

export type Transaction = {
  tx_interval: TimeInterval;
  tx_inputs: Input[];
};
export function Transaction(tx_interval: TimeInterval, tx_inputs: Input[]): Transaction {
  return { tx_interval, tx_inputs };
}
export namespace Transaction {
  export const jsonCodec: JsonCodec<Transaction> = objectOf({
    tx_interval: TimeInterval.jsonCodec,
    tx_inputs: arrayOf(Input.jsonCodec),
  });
  export const areEqual = (a: Transaction, b: Transaction): boolean =>
    TimeInterval.areEqual(a.tx_interval, b.tx_interval) &&
    arrayAreEqualWith(a.tx_inputs, b.tx_inputs, Input.areEqual);
}

export type SingleInputTx = {
  interval: TimeInterval;
  input?: Input;
};
export function SingleInputTx(interval: TimeInterval, input?: Input): SingleInputTx {
  return { interval, input };
}
export namespace SingleInputTx {
  export const areEqual = (a: SingleInputTx, b: SingleInputTx): boolean =>
    TimeInterval.areEqual(a.interval, b.interval) &&
    ((a.input === undefined && b.input === undefined) ||
      (a.input !== undefined && b.input !== undefined && Input.areEqual(a.input, b.input)));
}

export type NonPositiveDeposit = {
  party: Party;
  asked_to_deposit: bigint;
  of_token: Token;
  in_account: AccountId;
};
export function NonPositiveDeposit(party: Party, asked_to_deposit: bigint, of_token: Token, in_account: AccountId): NonPositiveDeposit {
  return { party, asked_to_deposit, of_token, in_account };
}
export namespace NonPositiveDeposit {
  export const jsonCodec: JsonCodec<NonPositiveDeposit> = objectOf({
    party: Party.jsonCodec,
    asked_to_deposit: json2BigIntCodec,
    of_token: Token.jsonCodec,
    in_account: AccountId.jsonCodec,
  });
  export const areEqual = (a: NonPositiveDeposit, b: NonPositiveDeposit): boolean =>
    Party.areEqual(a.party, b.party) &&
    a.asked_to_deposit === b.asked_to_deposit &&
    Token.areEqual(a.of_token, b.of_token) &&
    AccountId.areEqual(a.in_account, b.in_account);
}

export type NonPositivePay = {
  account: AccountId;
  asked_to_pay: bigint;
  of_token: Token;
  to_payee: Payee;
};
export function NonPositivePay(account: AccountId, asked_to_pay: bigint, of_token: Token, to_payee: Payee): NonPositivePay {
  return { account, asked_to_pay, of_token, to_payee };
}
export namespace NonPositivePay {
  export const jsonCodec: JsonCodec<NonPositivePay> = objectOf({
    account: AccountId.jsonCodec,
    asked_to_pay: json2BigIntCodec,
    of_token: Token.jsonCodec,
    to_payee: Payee.jsonCodec,
  });
  export const areEqual = (a: NonPositivePay, b: NonPositivePay): boolean =>
    AccountId.areEqual(a.account, b.account) &&
    a.asked_to_pay === b.asked_to_pay &&
    Token.areEqual(a.of_token, b.of_token) &&
    Payee.areEqual(a.to_payee, b.to_payee);
}

export type PartialPay = {
  account: AccountId;
  asked_to_pay: bigint;
  of_token: Token;
  to_payee: Payee;
  but_only_paid: bigint;
};
export function PartialPay(account: AccountId, asked_to_pay: bigint, of_token: Token, to_payee: Payee, but_only_paid: bigint): PartialPay {
  return { account, asked_to_pay, of_token, to_payee, but_only_paid };
}
export namespace PartialPay {
  export const jsonCodec: JsonCodec<PartialPay> = objectOf({
    account: AccountId.jsonCodec,
    asked_to_pay: json2BigIntCodec,
    of_token: Token.jsonCodec,
    to_payee: Payee.jsonCodec,
    but_only_paid: json2BigIntCodec,
  });
  export const areEqual = (a: PartialPay, b: PartialPay): boolean =>
    AccountId.areEqual(a.account, b.account) &&
    a.asked_to_pay === b.asked_to_pay &&
    Token.areEqual(a.of_token, b.of_token) &&
    Payee.areEqual(a.to_payee, b.to_payee) &&
    a.but_only_paid === b.but_only_paid;
}

export type Shadowing = {
  value_id: string;
  had_value: bigint;
  is_now_assigned: bigint;
};
export function Shadowing(value_id: string, had_value: bigint, is_now_assigned: bigint): Shadowing {
  return { value_id, had_value, is_now_assigned };
}
export namespace Shadowing {
  export const jsonCodec: JsonCodec<Shadowing> = objectOf({
    value_id: json2StringCodec,
    had_value: json2BigIntCodec,
    is_now_assigned: json2BigIntCodec,
  });
  export const areEqual = (a: Shadowing, b: Shadowing): boolean =>
    a.value_id === b.value_id &&
    a.had_value === b.had_value &&
    a.is_now_assigned === b.is_now_assigned;
}

export type AssertionFailed = "assertion_failed";
export function AssertionFailed(): AssertionFailed {
  return "assertion_failed";
}
export namespace AssertionFailed {
  export const jsonCodec: JsonCodec<AssertionFailed> = jsonConstant("assertion_failed");
  export const areEqual = (a: AssertionFailed, b: AssertionFailed): boolean => a === b;
}

export type TransactionWarning = NonPositiveDeposit | NonPositivePay | PartialPay | Shadowing | AssertionFailed;
export namespace TransactionWarning {
  export const isNonPositiveDeposit = (w: TransactionWarning): w is NonPositiveDeposit =>
    typeof w === "object" && w !== null && "party" in w;
  export const isNonPositivePay = (w: TransactionWarning): w is NonPositivePay =>
    typeof w === "object" && w !== null && "account" in w && "asked_to_pay" in w && !("but_only_paid" in w);
  export const isPartialPay = (w: TransactionWarning): w is PartialPay =>
    typeof w === "object" && w !== null && "but_only_paid" in w;
  export const isShadowing = (w: TransactionWarning): w is Shadowing =>
    typeof w === "object" && w !== null && "value_id" in w;
  export const isAssertionFailed = (w: TransactionWarning): w is AssertionFailed =>
    typeof w === "string";

  export const match = <T>(
    w: TransactionWarning,
    handlers: {
      non_positive_deposit: (v: NonPositiveDeposit) => T,
      non_positive_pay: (v: NonPositivePay) => T,
      partial_pay: (v: PartialPay) => T,
      shadowing: (v: Shadowing) => T,
      assertion_failed: (v: AssertionFailed) => T,
    },
  ): T =>
    isNonPositiveDeposit(w) ? handlers.non_positive_deposit(w)
    : isPartialPay(w) ? handlers.partial_pay(w)
    : isShadowing(w) ? handlers.shadowing(w)
    : isNonPositivePay(w) ? handlers.non_positive_pay(w)
    : handlers.assertion_failed(w);

  export const tryMatch = <T>(
    w: TransactionWarning,
    handlers: {
      non_positive_deposit?: (v: NonPositiveDeposit) => T,
      non_positive_pay?: (v: NonPositivePay) => T,
      partial_pay?: (v: PartialPay) => T,
      shadowing?: (v: Shadowing) => T,
      assertion_failed?: (v: AssertionFailed) => T,
    },
  ): Result<T, string> =>
    isNonPositiveDeposit(w) ? handlers.non_positive_deposit ? ok(handlers.non_positive_deposit(w)) : err("Missing non_positive_deposit handler")
    : isPartialPay(w) ? handlers.partial_pay ? ok(handlers.partial_pay(w)) : err("Missing partial_pay handler")
    : isShadowing(w) ? handlers.shadowing ? ok(handlers.shadowing(w)) : err("Missing shadowing handler")
    : isNonPositivePay(w) ? handlers.non_positive_pay ? ok(handlers.non_positive_pay(w)) : err("Missing non_positive_pay handler")
    : handlers.assertion_failed ? ok(handlers.assertion_failed(w)) : err("Missing assertion_failed handler");

  export const jsonCodec: JsonCodec<TransactionWarning> = altJsonCodecs(
    [
      NonPositiveDeposit.jsonCodec,
      NonPositivePay.jsonCodec,
      PartialPay.jsonCodec,
      Shadowing.jsonCodec,
      AssertionFailed.jsonCodec,
    ],
    (serNonPosDep, serNonPosPay, serPartial, serShadow, serAssert) => (w: TransactionWarning) => match(w, {
      non_positive_deposit: serNonPosDep,
      non_positive_pay: serNonPosPay,
      partial_pay: serPartial,
      shadowing: serShadow,
      assertion_failed: serAssert,
    })
  );
  export const areEqual = (a: TransactionWarning, b: TransactionWarning): boolean =>
    isAssertionFailed(a) ? isAssertionFailed(b) && AssertionFailed.areEqual(a, b)
    : isNonPositiveDeposit(a) ? isNonPositiveDeposit(b) && NonPositiveDeposit.areEqual(a, b)
    : isShadowing(a) ? isShadowing(b) && Shadowing.areEqual(a, b)
    : isPartialPay(a) ? isPartialPay(b) && PartialPay.areEqual(a, b)
    : isNonPositivePay(b) && NonPositivePay.areEqual(a, b);
}

export type InvalidInterval = {
  invalidInterval: { from: bigint; to: bigint };
};
export function InvalidInterval(invalidInterval: { from: bigint; to: bigint }): InvalidInterval {
  return { invalidInterval };
}
export namespace InvalidInterval {
  export const jsonCodec: JsonCodec<InvalidInterval> = objectOf({
    invalidInterval: objectOf({ from: json2BigIntCodec, to: json2BigIntCodec }),
  });
  export const areEqual = (a: InvalidInterval, b: InvalidInterval): boolean =>
    a.invalidInterval.from === b.invalidInterval.from &&
    a.invalidInterval.to === b.invalidInterval.to;
}

export type IntervalInPast = {
  intervalInPastError: { from: bigint; to: bigint; minTime: bigint };
};
export function IntervalInPast(intervalInPastError: { from: bigint; to: bigint; minTime: bigint }): IntervalInPast {
  return { intervalInPastError };
}
export namespace IntervalInPast {
  export const jsonCodec: JsonCodec<IntervalInPast> = objectOf({
    intervalInPastError: objectOf({
      from: json2BigIntCodec,
      to: json2BigIntCodec,
      minTime: json2BigIntCodec,
    }),
  });
  export const areEqual = (a: IntervalInPast, b: IntervalInPast): boolean =>
    a.intervalInPastError.from === b.intervalInPastError.from &&
    a.intervalInPastError.to === b.intervalInPastError.to &&
    a.intervalInPastError.minTime === b.intervalInPastError.minTime;
}

export type IntervalError = InvalidInterval | IntervalInPast;
export namespace IntervalError {
  export const isInvalidInterval = (e: IntervalError): e is InvalidInterval =>
    typeof e === "object" && e !== null && "invalidInterval" in e;
  export const isIntervalInPast = (e: IntervalError): e is IntervalInPast =>
    typeof e === "object" && e !== null && "intervalInPastError" in e;

  export const match = <T>(
    e: IntervalError,
    handlers: {
      invalid_interval: (v: InvalidInterval) => T,
      interval_in_past: (v: IntervalInPast) => T,
    },
  ): T =>
    isInvalidInterval(e) ? handlers.invalid_interval(e) : handlers.interval_in_past(e);

  export const tryMatch = <T>(
    e: IntervalError,
    handlers: {
      invalid_interval?: (v: InvalidInterval) => T,
      interval_in_past?: (v: IntervalInPast) => T,
    },
  ): Result<T, string> =>
    isInvalidInterval(e) ? handlers.invalid_interval ? ok(handlers.invalid_interval(e)) : err("Missing invalid_interval handler")
    : handlers.interval_in_past ? ok(handlers.interval_in_past(e)) : err("Missing interval_in_past handler");

  export const jsonCodec: JsonCodec<IntervalError> = altJsonCodecs(
    [InvalidInterval.jsonCodec, IntervalInPast.jsonCodec],
    (serInvalid, serPast) => (e: IntervalError) => match(e, {
      invalid_interval: serInvalid,
      interval_in_past: serPast,
    })
  );
  export const areEqual = (a: IntervalError, b: IntervalError): boolean =>
    isInvalidInterval(a) ? isInvalidInterval(b) && InvalidInterval.areEqual(a, b)
    : isIntervalInPast(b) && IntervalInPast.areEqual(a, b);
}

export type AmbiguousTimeIntervalError = "TEAmbiguousTimeIntervalError";
export function AmbiguousTimeIntervalError(): AmbiguousTimeIntervalError {
  return "TEAmbiguousTimeIntervalError";
}
export namespace AmbiguousTimeIntervalError {
  export const jsonCodec: JsonCodec<AmbiguousTimeIntervalError> = jsonConstant("TEAmbiguousTimeIntervalError");
  export const areEqual = (a: AmbiguousTimeIntervalError, b: AmbiguousTimeIntervalError): boolean => a === b;
}

export type ApplyNoMatchError = "TEApplyNoMatchError";
export function ApplyNoMatchError(): ApplyNoMatchError {
  return "TEApplyNoMatchError";
}
export namespace ApplyNoMatchError {
  export const jsonCodec: JsonCodec<ApplyNoMatchError> = jsonConstant("TEApplyNoMatchError");
  export const areEqual = (a: ApplyNoMatchError, b: ApplyNoMatchError): boolean => a === b;
}

export type UselessTransaction = "TEUselessTransaction";
export function UselessTransaction(): UselessTransaction {
  return "TEUselessTransaction";
}
export namespace UselessTransaction {
  export const jsonCodec: JsonCodec<UselessTransaction> = jsonConstant("TEUselessTransaction");
  export const areEqual = (a: UselessTransaction, b: UselessTransaction): boolean => a === b;
}

export type HashMismatchError = "TEHashMismatch";
export function HashMismatchError(): HashMismatchError {
  return "TEHashMismatch";
}
export namespace HashMismatchError {
  export const jsonCodec: JsonCodec<HashMismatchError> = jsonConstant("TEHashMismatch");
  export const areEqual = (a: HashMismatchError, b: HashMismatchError): boolean => a === b;
}

export type TEIntervalError = {
  error: "TEIntervalError";
  context: IntervalError;
};
export function TEIntervalError(context: IntervalError): TEIntervalError {
  return { error: "TEIntervalError", context };
}
export namespace TEIntervalError {
  export const jsonCodec: JsonCodec<TEIntervalError> = objectOf({
    error: jsonConstant("TEIntervalError"),
    context: IntervalError.jsonCodec,
  });
  export const areEqual = (a: TEIntervalError, b: TEIntervalError): boolean =>
    a.error === b.error && IntervalError.areEqual(a.context, b.context);
}

export type TransactionError =
  | AmbiguousTimeIntervalError
  | ApplyNoMatchError
  | UselessTransaction
  | TEIntervalError
  | HashMismatchError;
export namespace TransactionError {
  export const isAmbiguousTimeIntervalError = (e: TransactionError): e is AmbiguousTimeIntervalError =>
    e === "TEAmbiguousTimeIntervalError";
  export const isApplyNoMatchError = (e: TransactionError): e is ApplyNoMatchError =>
    e === "TEApplyNoMatchError";
  export const isUselessTransaction = (e: TransactionError): e is UselessTransaction =>
    e === "TEUselessTransaction";
  export const isHashMismatchError = (e: TransactionError): e is HashMismatchError =>
    e === "TEHashMismatch";
  export const isTEIntervalError = (e: TransactionError): e is TEIntervalError =>
    typeof e === "object" && e !== null && "context" in e;

  export const match = <T>(
    e: TransactionError,
    handlers: {
      ambiguous_time_interval: (v: AmbiguousTimeIntervalError) => T,
      apply_no_match: (v: ApplyNoMatchError) => T,
      useless_transaction: (v: UselessTransaction) => T,
      hash_mismatch: (v: HashMismatchError) => T,
      te_interval: (v: TEIntervalError) => T,
    },
  ): T =>
    isAmbiguousTimeIntervalError(e) ? handlers.ambiguous_time_interval(e)
    : isApplyNoMatchError(e) ? handlers.apply_no_match(e)
    : isUselessTransaction(e) ? handlers.useless_transaction(e)
    : isHashMismatchError(e) ? handlers.hash_mismatch(e)
    : handlers.te_interval(e);

  export const tryMatch = <T>(
    e: TransactionError,
    handlers: {
      ambiguous_time_interval?: (v: AmbiguousTimeIntervalError) => T,
      apply_no_match?: (v: ApplyNoMatchError) => T,
      useless_transaction?: (v: UselessTransaction) => T,
      hash_mismatch?: (v: HashMismatchError) => T,
      te_interval?: (v: TEIntervalError) => T,
    },
  ): Result<T, string> =>
    isAmbiguousTimeIntervalError(e) ? handlers.ambiguous_time_interval ? ok(handlers.ambiguous_time_interval(e)) : err("Missing ambiguous_time_interval handler")
    : isApplyNoMatchError(e) ? handlers.apply_no_match ? ok(handlers.apply_no_match(e)) : err("Missing apply_no_match handler")
    : isUselessTransaction(e) ? handlers.useless_transaction ? ok(handlers.useless_transaction(e)) : err("Missing useless_transaction handler")
    : isHashMismatchError(e) ? handlers.hash_mismatch ? ok(handlers.hash_mismatch(e)) : err("Missing hash_mismatch handler")
    : handlers.te_interval ? ok(handlers.te_interval(e)) : err("Missing te_interval handler");

  export const jsonCodec: JsonCodec<TransactionError> = fromCodecThunkFn(() =>
    altJsonCodecs(
      [
        AmbiguousTimeIntervalError.jsonCodec,
        ApplyNoMatchError.jsonCodec,
        UselessTransaction.jsonCodec,
        TEIntervalError.jsonCodec,
        HashMismatchError.jsonCodec,
      ],
      (serAmbiguous, serNoMatch, serUseless, serInterval, serHash) => (e: TransactionError) => match(e, {
        ambiguous_time_interval: serAmbiguous,
        apply_no_match: serNoMatch,
        useless_transaction: serUseless,
        hash_mismatch: serHash,
        te_interval: serInterval,
      })
    )
  );
  export const areEqual = (a: TransactionError, b: TransactionError): boolean =>
    isAmbiguousTimeIntervalError(a) ? isAmbiguousTimeIntervalError(b) && AmbiguousTimeIntervalError.areEqual(a, b)
    : isApplyNoMatchError(a) ? isApplyNoMatchError(b) && ApplyNoMatchError.areEqual(a, b)
    : isUselessTransaction(a) ? isUselessTransaction(b) && UselessTransaction.areEqual(a, b)
    : isHashMismatchError(a) ? isHashMismatchError(b) && HashMismatchError.areEqual(a, b)
    : isTEIntervalError(b) && TEIntervalError.areEqual(a, b);
}

export type TransactionSuccess = {
  warnings: TransactionWarning[];
  payments: Payment[];
  state: MarloweState;
  contract: Contract;
};
export function TransactionSuccess(warnings: TransactionWarning[], payments: Payment[], state: MarloweState, contract: Contract): TransactionSuccess {
  return { warnings, payments, state, contract };
}
export namespace TransactionSuccess {
  export const jsonCodec: JsonCodec<TransactionSuccess> = objectOf({
    warnings: arrayOf(TransactionWarning.jsonCodec),
    payments: arrayOf(Payment.jsonCodec),
    state: MarloweState.jsonCodec,
    contract: Contract.jsonCodec,
  });
  export const areEqual = (a: TransactionSuccess, b: TransactionSuccess): boolean =>
    arrayAreEqualWith(a.warnings, b.warnings, TransactionWarning.areEqual) &&
    arrayAreEqualWith(a.payments, b.payments, Payment.areEqual) &&
    MarloweState.areEqual(a.state, b.state) &&
    Contract.areEqual(a.contract, b.contract);
}

export type TransactionErrorCase = { transaction_error: TransactionError };
export function TransactionErrorCase(transaction_error: TransactionError): TransactionErrorCase {
  return { transaction_error };
}
export namespace TransactionErrorCase {
  export const jsonCodec: JsonCodec<TransactionErrorCase> = objectOf({
    transaction_error: TransactionError.jsonCodec,
  });
  export const areEqual = (a: TransactionErrorCase, b: TransactionErrorCase): boolean =>
    TransactionError.areEqual(a.transaction_error, b.transaction_error);
}

export type TransactionOutput = TransactionSuccess | TransactionErrorCase;
export namespace TransactionOutput {
  export const isTransactionSuccess = (o: TransactionOutput): o is TransactionSuccess =>
    typeof o === "object" && o !== null && !("transaction_error" in o);
  export const isTransactionErrorCase = (o: TransactionOutput): o is TransactionErrorCase =>
    typeof o === "object" && o !== null && "transaction_error" in o;

  export const match = <T>(
    o: TransactionOutput,
    handlers: {
      success: (v: TransactionSuccess) => T,
      error: (v: TransactionErrorCase) => T,
    },
  ): T =>
    isTransactionErrorCase(o) ? handlers.error(o) : handlers.success(o);

  export const tryMatch = <T>(
    o: TransactionOutput,
    handlers: {
      success?: (v: TransactionSuccess) => T,
      error?: (v: TransactionErrorCase) => T,
    },
  ): Result<T, string> =>
    isTransactionErrorCase(o) ? handlers.error ? ok(handlers.error(o)) : err("Missing error handler")
    : handlers.success ? ok(handlers.success(o)) : err("Missing success handler");

  export const jsonCodec: JsonCodec<TransactionOutput> = altJsonCodecs(
    [TransactionSuccess.jsonCodec, TransactionErrorCase.jsonCodec],
    (serSuccess, serError) => (o: TransactionOutput) => match(o, {
      success: serSuccess,
      error: serError,
    })
  );
  export const areEqual = (a: TransactionOutput, b: TransactionOutput): boolean =>
    isTransactionErrorCase(a) ? isTransactionErrorCase(b) && TransactionErrorCase.areEqual(a, b)
    : isTransactionErrorCase(b) ? false : TransactionSuccess.areEqual(a, b);
}