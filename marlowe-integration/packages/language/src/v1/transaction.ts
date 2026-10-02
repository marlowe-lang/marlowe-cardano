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
import type { Contract } from "./contract.js";
import { Contract as ContractNamespace } from "./contract.js";
import type { TimeInterval } from "./environment.js";
import { TimeInterval as TimeIntervalNamespace } from "./environment.js";
import type { Input } from "./inputs.js";
import { Input as InputNamespace } from "./inputs.js";
import type { Party } from "./participants.js";
import { Party as PartyNamespace } from "./participants.js";
import type { AccountId, Payee } from "./payee.js";
import { AccountId as AccountIdNamespace, Payee as PayeeNamespace } from "./payee.js";
import type { MarloweState } from "./state.js";
import { MarloweState as MarloweStateNamespace } from "./state.js";
import type { Token } from "./token.js";
import { Token as TokenNamespace } from "./token.js";

// ---- Payment ----------------------------------------------------------------

export type Payment = {
  payment_from: AccountId;
  to: Payee;
  amount: bigint;
  token: Token;
};

export namespace Payment {
  export const jsonCodec: JsonCodec<Payment> = objectOf({
    payment_from: AccountIdNamespace.jsonCodec,
    to: PayeeNamespace.jsonCodec,
    amount: json2BigIntCodec,
    token: TokenNamespace.jsonCodec,
  });
}

// ---- Transaction ------------------------------------------------------------

export type Transaction = {
  tx_interval: TimeInterval;
  tx_inputs: Input[];
};

export namespace Transaction {
  export const jsonCodec: JsonCodec<Transaction> = objectOf({
    tx_interval: TimeIntervalNamespace.jsonCodec,
    tx_inputs: arrayOf(InputNamespace.jsonCodec),
  });
}

// ---- SingleInputTx ----------------------------------------------------------

export type SingleInputTx = {
  interval: TimeInterval;
  input?: Input;
};

// ---- TransactionWarning -----------------------------------------------------

export type NonPositiveDeposit = {
  party: Party;
  asked_to_deposit: bigint;
  of_token: Token;
  in_account: AccountId;
};

export namespace NonPositiveDeposit {
  export const jsonCodec: JsonCodec<NonPositiveDeposit> = objectOf({
    party: PartyNamespace.jsonCodec,
    asked_to_deposit: json2BigIntCodec,
    of_token: TokenNamespace.jsonCodec,
    in_account: AccountIdNamespace.jsonCodec,
  });
}

export type NonPositivePay = {
  account: AccountId;
  asked_to_pay: bigint;
  of_token: Token;
  to_payee: Payee;
};

export namespace NonPositivePay {
  export const jsonCodec: JsonCodec<NonPositivePay> = objectOf({
    account: AccountIdNamespace.jsonCodec,
    asked_to_pay: json2BigIntCodec,
    of_token: TokenNamespace.jsonCodec,
    to_payee: PayeeNamespace.jsonCodec,
  });
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
    account: AccountIdNamespace.jsonCodec,
    asked_to_pay: json2BigIntCodec,
    of_token: TokenNamespace.jsonCodec,
    to_payee: PayeeNamespace.jsonCodec,
    but_only_paid: json2BigIntCodec,
  });
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
}

export type AssertionFailed = "assertion_failed";

export namespace AssertionFailed {
  export const jsonCodec: JsonCodec<AssertionFailed> = jsonConstant("assertion_failed");
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
}

// ---- IntervalError ---------------------------------------------------------

export type InvalidInterval = {
  invalidInterval: { from: bigint; to: bigint };
};

export namespace InvalidInterval {
  export const jsonCodec: JsonCodec<InvalidInterval> = objectOf({
    invalidInterval: objectOf({ from: json2BigIntCodec, to: json2BigIntCodec }),
  });
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
}

export type IntervalError = InvalidInterval | IntervalInPast;

export namespace IntervalError {
  export const jsonCodec: JsonCodec<IntervalError> = altJsonCodecs(
    [InvalidInterval.jsonCodec, IntervalInPast.jsonCodec],
    (serInvalid, serPast) => (e: IntervalError) =>
      "invalidInterval" in e ? serInvalid(e) : serPast(e)
  );
}

// ---- TransactionError ------------------------------------------------------

export type AmbiguousTimeIntervalError = "TEAmbiguousTimeIntervalError";

export namespace AmbiguousTimeIntervalError {
  export const jsonCodec: JsonCodec<AmbiguousTimeIntervalError> = jsonConstant("TEAmbiguousTimeIntervalError");
}

export type ApplyNoMatchError = "TEApplyNoMatchError";

export namespace ApplyNoMatchError {
  export const jsonCodec: JsonCodec<ApplyNoMatchError> = jsonConstant("TEApplyNoMatchError");
}

export type UselessTransaction = "TEUselessTransaction";

export namespace UselessTransaction {
  export const jsonCodec: JsonCodec<UselessTransaction> = jsonConstant("TEUselessTransaction");
}

export type HashMismatchError = "TEHashMismatch";

export namespace HashMismatchError {
  export const jsonCodec: JsonCodec<HashMismatchError> = jsonConstant("TEHashMismatch");
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
    state: MarloweStateNamespace.jsonCodec,
    contract: ContractNamespace.jsonCodec,
  });
}

export namespace TransactionErrorCase {
  export const jsonCodec: JsonCodec<TransactionErrorCase> = objectOf({
    transaction_error: TransactionError.jsonCodec,
  });
}

export namespace TransactionOutput {
  export const jsonCodec: JsonCodec<TransactionOutput> = altJsonCodecs(
    [TransactionSuccess.jsonCodec, TransactionErrorCase.jsonCodec],
    (serSuccess, serError) => (o: TransactionOutput) =>
      "transaction_error" in o ? serError(o) : serSuccess(o)
  );
}