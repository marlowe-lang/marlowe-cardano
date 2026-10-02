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
import type { Observation, Value, ValueId } from "./value-and-observation.js";
import { Observation as ObservationNamespace, Value as ValueNamespace } from "./value-and-observation.js";
import type { AccountId, Payee } from "./payee.js";
import { AccountId as AccountIdNamespace, Payee as PayeeNamespace } from "./payee.js";
import type { Token } from "./token.js";
import { Token as TokenNamespace } from "./token.js";
import type { Action } from "./actions.js";
import { Action as ActionNamespace } from "./actions.js";

// Namespaces are sensitive to the order of declaration, so the recursive
// `Contract` namespace is declared first via `fromCodecThunkFn`. The other
// namespaces (`Pay`, `If`, `Let`, `Assert`, `NormalCase`, `When`) then
// capture the codec reference at definition time. `objectOf` only stores
// field codecs — the recursion is only triggered when the codec is
// actually invoked, by which time the thunk has been resolved.

// Core `Contract` type --------------------------------------------------------

export type Close = "close";

export type Pay = {
  pay: Value;
  token: Token;
  from_account: AccountId;
  to: Payee;
  then: Contract;
};

export type If = {
  if: Observation;
  then: Contract;
  else: Contract;
};

export type Let = {
  let: ValueId;
  be: Value;
  then: Contract;
};

export type Assert = {
  assert: Observation;
  then: Contract;
};

export type When = {
  when: Case[];
  timeout: Timeout;
  timeout_continuation: Contract;
};

export type NormalCase = NormalCaseImpl;
export type MerkleizedCase = MerkleizedCaseImpl;
export type Case = NormalCase | MerkleizedCase;
export type Timeout = bigint;
export type Contract = Close | Pay | If | When | Let | Assert;

// Internal type aliases used to keep the implementation types and the public
// type names distinct while we wire up the codec namespaces. (TS merges the
// `type X = ...` and `namespace X { ... }` declarations, so we cannot reuse
// the same name for both a recursive type alias and a non-recursive impl.)
type NormalCaseImpl = {
  case: Action;
  then: Contract;
};
type MerkleizedCaseImpl = {
  case: Action;
  merkleized_then: string;
};

// `Contract.jsonCodec` is declared first via `fromCodecThunkFn` so that the
// other namespaces (`Pay`, `If`, `Let`, `Assert`, `NormalCase`, `When`)
// can capture the codec reference at definition time.

export namespace Contract {
  export const jsonCodec: JsonCodec<Contract> = fromCodecThunkFn(() =>
    altJsonCodecs<[JsonCodec<Close>, JsonCodec<Pay>, JsonCodec<If>, JsonCodec<When>, JsonCodec<Let>, JsonCodec<Assert>]>(
      [Close.jsonCodec, Pay.jsonCodec, If.jsonCodec, When.jsonCodec, Let.jsonCodec, Assert.jsonCodec],
      (serClose, serPay, serIf, serWhen, serLet, serAssert) => (c: Contract) => {
        if (c === "close") return serClose(c);
        if ("pay" in c) return serPay(c);
        if ("when" in c) return serWhen(c);
        if ("if" in c) return serIf(c);
        if ("assert" in c) return serAssert(c);
        return serLet(c);
      }
    )
  );
}

// Smart constructors — same name as the type and the namespace.
// `function` declarations (not `const`) are required so they can share
// the name with the type/namespace triple.

export function Close(): Close {
  return "close";
}
export namespace Close {
  export const jsonCodec: JsonCodec<Close> = jsonConstant("close");
}

export function Pay(
  pay: Value,
  token: Token,
  from_account: AccountId,
  to: Payee,
  then: Contract
): Pay {
  return { pay, token, from_account, to, then };
}
export namespace Pay {
  export const jsonCodec: JsonCodec<Pay> = objectOf({
    pay: ValueNamespace.jsonCodec,
    token: TokenNamespace.jsonCodec,
    from_account: AccountIdNamespace.jsonCodec,
    to: PayeeNamespace.jsonCodec,
    then: Contract.jsonCodec,
  });
}

export function If(ifObs: Observation, thenCont: Contract, elseCont: Contract): If {
  return { if: ifObs, then: thenCont, else: elseCont };
}
export namespace If {
  export const jsonCodec: JsonCodec<If> = objectOf({
    if: ObservationNamespace.jsonCodec,
    then: Contract.jsonCodec,
    else: Contract.jsonCodec,
  });
}

export function Let(letId: ValueId, be: Value, then: Contract): Let {
  return { let: letId, be, then };
}
export namespace Let {
  export const jsonCodec: JsonCodec<Let> = objectOf({
    let: json2StringCodec,
    be: ValueNamespace.jsonCodec,
    then: Contract.jsonCodec,
  });
}

export function Assert(assertObs: Observation, then: Contract): Assert {
  return { assert: assertObs, then };
}
export namespace Assert {
  export const jsonCodec: JsonCodec<Assert> = objectOf({
    assert: ObservationNamespace.jsonCodec,
    then: Contract.jsonCodec,
  });
}

export namespace Timeout {
  export const jsonCodec: JsonCodec<Timeout> = json2BigIntCodec;
}

export function datetoTimeout(date: Date): Timeout {
  return BigInt(Math.floor(date.getTime() / 1000) * 1000);
}

export function timeoutToDate(timeout: Timeout): Date {
  return new Date(Number(timeout));
}

export namespace NormalCase {
  export const jsonCodec: JsonCodec<NormalCase> = objectOf({
    case: ActionNamespace.jsonCodec,
    then: Contract.jsonCodec,
  });
}

export function MerkleizedCase(caseAction: Action, merkleized_then: string): Case {
  return { case: caseAction, merkleized_then };
}
export namespace MerkleizedCase {
  export const jsonCodec: JsonCodec<MerkleizedCase> = objectOf({
    case: ActionNamespace.jsonCodec,
    merkleized_then: json2StringCodec,
  });
}

export function Case(caseAction: Action, continuation: Contract): Case {
  return { case: caseAction, then: continuation };
}
export namespace Case {
  export const jsonCodec: JsonCodec<Case> = altJsonCodecs(
    [NormalCase.jsonCodec, MerkleizedCase.jsonCodec],
    (serNormal, serMerkleized) => (c: Case) =>
      "merkleized_then" in c ? serMerkleized(c) : serNormal(c)
  );
}

export function When(cases: Case[], timeout: Timeout, timeoutCont: Contract): When {
  return { when: cases, timeout, timeout_continuation: timeoutCont };
}
export namespace When {
  export const jsonCodec: JsonCodec<When> = objectOf({
    when: arrayOf(Case.jsonCodec),
    timeout: Timeout.jsonCodec,
    timeout_continuation: Contract.jsonCodec,
  });
}