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
import { arrayAreEqualWith } from "../assoc-map.js";
import { Contract } from "./contract.js";
import { TimeInterval } from "./environment.js";
import { Input } from "./inputs.js";
import { Party } from "./participants.js";
import { AccountId, Payee } from "./payee.js";
import { MarloweState } from "./state.js";
import { Token } from "./token.js";

// ---- Payment ----------------------------------------------------------------

export type Payment = {
  payment_from: AccountId;
  to: Payee;
  amount: bigint;
  token: Token;
};

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

// ---- Transaction ------------------------------------------------------------

export type Transaction = {
  tx_interval: TimeInterval;
  tx_inputs: Input[];
};

export namespace Transaction {
  export const jsonCodec: JsonCodec<Transaction> = objectOf({
    tx_interval: TimeInterval.jsonCodec,
    tx_inputs: arrayOf(Input.jsonCodec),
  });
  export const areEqual = (a: Transaction, b: Transaction): boolean =>
    TimeInterval.areEqual(a.tx_interval, b.tx_interval) &&
    arrayAreEqualWith(a.tx_inputs, b.tx_inputs, Input.areEqual);
}

// ---- SingleInputTx ----------------------------------------------------------

export type SingleInputTx = {
  interval: TimeInterval;
  input?: Input;
};

export namespace SingleInputTx {
  export const areEqual = (a: SingleInputTx, b: SingleInputTx): boolean =>
    TimeInterval.areEqual(a.interval, b.interval) &&
    ((a.input === undefined && b.input === undefined) ||
      (a.input !== undefined && b.input !== undefined && Input.areEqual(a.input, b.input)));
}

// ---- TransactionWarning -----------------------------------------------------

export type NonPositiveDeposit = {
  party: Party;
  asked_to_deposit: bigint;
  of_token: Token;
  in_account: AccountId;
};

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

export namespace AssertionFailed {
  export const jsonCodec: JsonCodec<AssertionFailed> = jsonConstant("assertion_failed");
  export const areEqual = (a: AssertionFailed, b: AssertionFailed): boolean => a === b;
}

export type TransactionWarning = NonPositiveDeposit | NonPositivePay | PartialPay | Shadowing | AssertionFailed;

export namespace TransactionWarning {
  export const jsonCodec: JsonCodec<TransactionWarning> = altJsonCodecs(
    [
      NonPositiveDeposit.jsonCodec,
      NonPositivePay.jsonCodec,
      PartialPay.jsonCodec,
      Shadowing.jsonCodec,
      AssertionFailed.jsonCodec,
    ],
    (serNonPosDep, serNonPosPay, serPartial, serShadow, serAssert) => (w: TransactionWarning) => {
      if (typeof w === "string") return serAssert(w);
      if ("party" in w) return serNonPosDep(w);
      if ("account" in w && "asked_to_pay" in w && "but_only_paid" in w) return serPartial(w);
      if ("account" in w) return serNonPosPay(w);
      return serShadow(w);
    }
  );
  export const areEqual = (a: TransactionWarning, b: TransactionWarning): boolean => {
    if (typeof a === "string") return typeof b === "string" && a === b;
    if ("party" in a) return "party" in b && NonPositiveDeposit.areEqual(a, b);
    if ("value_id" in a) return "value_id" in b && Shadowing.areEqual(a, b);
    if ("but_only_paid" in a) return "but_only_paid" in b && PartialPay.areEqual(a, b);
    return "account" in b && NonPositivePay.areEqual(a, b);
  };
}

// ---- IntervalError ---------------------------------------------------------

export type InvalidInterval = {
  invalidInterval: { from: bigint; to: bigint };
};

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
  export const jsonCodec: JsonCodec<IntervalError> = altJsonCodecs(
    [InvalidInterval.jsonCodec, IntervalInPast.jsonCodec],
    (serInvalid, serPast) => (e: IntervalError) =>
      "invalidInterval" in e ? serInvalid(e) : serPast(e)
  );
  export const areEqual = (a: IntervalError, b: IntervalError): boolean => {
    if ("invalidInterval" in a) return "invalidInterval" in b && InvalidInterval.areEqual(a, b);
    return "intervalInPastError" in b && IntervalInPast.areEqual(a, b);
  };
}

// ---- TransactionError ------------------------------------------------------

export type AmbiguousTimeIntervalError = "TEAmbiguousTimeIntervalError";

export namespace AmbiguousTimeIntervalError {
  export const jsonCodec: JsonCodec<AmbiguousTimeIntervalError> = jsonConstant("TEAmbiguousTimeIntervalError");
  export const areEqual = (a: AmbiguousTimeIntervalError, b: AmbiguousTimeIntervalError): boolean => a === b;
}

export type ApplyNoMatchError = "TEApplyNoMatchError";

export namespace ApplyNoMatchError {
  export const jsonCodec: JsonCodec<ApplyNoMatchError> = jsonConstant("TEApplyNoMatchError");
  export const areEqual = (a: ApplyNoMatchError, b: ApplyNoMatchError): boolean => a === b;
}

export type UselessTransaction = "TEUselessTransaction";

export namespace UselessTransaction {
  export const jsonCodec: JsonCodec<UselessTransaction> = jsonConstant("TEUselessTransaction");
  export const areEqual = (a: UselessTransaction, b: UselessTransaction): boolean => a === b;
}

export type HashMismatchError = "TEHashMismatch";

export namespace HashMismatchError {
  export const jsonCodec: JsonCodec<HashMismatchError> = jsonConstant("TEHashMismatch");
  export const areEqual = (a: HashMismatchError, b: HashMismatchError): boolean => a === b;
}

export type TEIntervalError = {
  error: "TEIntervalError";
  context: IntervalError;
};

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
  export const jsonCodec: JsonCodec<TransactionError> = fromCodecThunkFn(() =>
    altJsonCodecs(
      [
        AmbiguousTimeIntervalError.jsonCodec,
        ApplyNoMatchError.jsonCodec,
        UselessTransaction.jsonCodec,
        TEIntervalError.jsonCodec,
        HashMismatchError.jsonCodec,
      ],
      (serAmbiguous, serNoMatch, serUseless, serInterval, serHash) => (e: TransactionError) => {
        if (e === "TEAmbiguousTimeIntervalError") return serAmbiguous(e);
        if (e === "TEApplyNoMatchError") return serNoMatch(e);
        if (e === "TEUselessTransaction") return serUseless(e);
        if (e === "TEHashMismatch") return serHash(e);
        return serInterval(e);
      }
    )
  );
  export const areEqual = (a: TransactionError, b: TransactionError): boolean => {
    if (typeof a === "string") return typeof b === "string" && a === b;
    return "context" in b && TEIntervalError.areEqual(a, b);
  };
}

// ---- TransactionOutput -----------------------------------------------------

export type TransactionSuccess = {
  warnings: TransactionWarning[];
  payments: Payment[];
  state: MarloweState;
  contract: Contract;
};

export type TransactionErrorCase = { transaction_error: TransactionError };

export type TransactionOutput = TransactionSuccess | TransactionErrorCase;

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

export namespace TransactionErrorCase {
  export const jsonCodec: JsonCodec<TransactionErrorCase> = objectOf({
    transaction_error: TransactionError.jsonCodec,
  });
  export const areEqual = (a: TransactionErrorCase, b: TransactionErrorCase): boolean =>
    TransactionError.areEqual(a.transaction_error, b.transaction_error);
}

export namespace TransactionOutput {
  export const jsonCodec: JsonCodec<TransactionOutput> = altJsonCodecs(
    [TransactionSuccess.jsonCodec, TransactionErrorCase.jsonCodec],
    (serSuccess, serError) => (o: TransactionOutput) =>
      "transaction_error" in o ? serError(o) : serSuccess(o)
  );
  export const areEqual = (a: TransactionOutput, b: TransactionOutput): boolean => {
    if ("transaction_error" in a) {
      return "transaction_error" in b && TransactionErrorCase.areEqual(a, b);
    }
    return "transaction_error" in b ? false : TransactionSuccess.areEqual(a, b);
  };
}