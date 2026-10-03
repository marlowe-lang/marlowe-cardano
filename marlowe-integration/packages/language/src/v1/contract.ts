import {
  altJsonCodecs,
  arrayOf,
  constant as jsonConstant,
  json2BigIntCodec,
  json2StringCodec,
  objectOf,
  type JsonCodec,
  type JsonError,
} from "@konduit/codec/json/codecs";
import { fromCodecThunkFn } from "@konduit/codec";
import { areEqualThunk, arrayAreEqualWith } from "../assoc-map.js";
import { Observation, Value, ValueId } from "./value-and-observation.js";
import { AccountId, Payee } from "./payee.js";
import { Token } from "./token.js";
import { Action } from "./actions.js";

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
export type Contract = Assert | Close | If | Let | Pay | When;

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

export namespace Contract {
  export const isAssert = (contract: Contract): contract is Assert => typeof contract === "object" && "assert" in contract;
  export const isClose = (contract: Contract): contract is Close => contract === "close";
  export const isIf = (contract: Contract): contract is If => typeof contract === "object" && "if" in contract;
  export const isLet = (contract: Contract): contract is Let => typeof contract === "object" && "let" in contract;
  export const isPay = (contract: Contract): contract is Pay => typeof contract === "object" && "pay" in contract;
  export const isWhen = (contract: Contract): contract is When => typeof contract === "object" && "when" in contract;

  export const match = <T>(contract: Contract, handlers: { assert: (assert: Assert) => T, close: (close: Close) => T, if: (if_: If) => T, let: (let_: Let) => T, pay: (pay: Pay) => T, when: (when: When) => T }): T =>
    isAssert(contract) ? handlers.assert(contract)
    : isClose(contract) ? handlers.close(contract)
    : isIf(contract) ? handlers.if(contract)
    : isLet(contract) ? handlers.let(contract)
    : isPay(contract) ? handlers.pay(contract)
    : handlers.when(contract)

  export const areEqual = areEqualThunk<Contract>(() => (a, b) =>
    isAssert(a) ? isAssert(b) && Assert.areEqual(a, b)
    : isClose(a) ? isClose(b) && Close.areEqual(a, b)
    : isIf(a) ? isIf(b) && If.areEqual(a, b)
    : isLet(a) ? isLet(b) && Let.areEqual(a, b)
    : isPay(a) ? isPay(b) && Pay.areEqual(a, b)
    : isWhen(b) && When.areEqual(a, b)
  );

  export const jsonCodec: JsonCodec<Contract> = fromCodecThunkFn(() =>
    altJsonCodecs<[JsonCodec<Close>, JsonCodec<Pay>, JsonCodec<If>, JsonCodec<When>, JsonCodec<Let>, JsonCodec<Assert>]>(
      [Close.jsonCodec, Pay.jsonCodec, If.jsonCodec, When.jsonCodec, Let.jsonCodec, Assert.jsonCodec],
      (serClose, serPay, serIf, serWhen, serLet, serAssert) => (contract: Contract) => match(contract, {
          close: serClose,
          pay: serPay,
          if: serIf,
          when: serWhen,
          let: serLet,
          assert: serAssert
      })
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
  export const areEqual = (a: Close, b: Close): boolean => a === b;
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
    pay: Value.jsonCodec,
    token: Token.jsonCodec,
    from_account: AccountId.jsonCodec,
    to: Payee.jsonCodec,
    then: Contract.jsonCodec,
  });
  export const areEqual = (a: Pay, b: Pay): boolean =>
    Value.areEqual(a.pay, b.pay) &&
    Token.areEqual(a.token, b.token) &&
    AccountId.areEqual(a.from_account, b.from_account) &&
    Payee.areEqual(a.to, b.to) &&
    Contract.areEqual(a.then, b.then);
}

export function If(ifObs: Observation, thenCont: Contract, elseCont: Contract): If {
  return { if: ifObs, then: thenCont, else: elseCont };
}
export namespace If {
  export const jsonCodec: JsonCodec<If> = objectOf({
    if: Observation.jsonCodec,
    then: Contract.jsonCodec,
    else: Contract.jsonCodec,
  });
  export const areEqual = (a: If, b: If): boolean =>
    Observation.areEqual(a.if, b.if) &&
    Contract.areEqual(a.then, b.then) &&
    Contract.areEqual(a.else, b.else);
}

export function Let(letId: ValueId, be: Value, then: Contract): Let {
  return { let: letId, be, then };
}
export namespace Let {
  export const jsonCodec: JsonCodec<Let> = objectOf({
    let: json2StringCodec,
    be: Value.jsonCodec,
    then: Contract.jsonCodec,
  });
  export const areEqual = (a: Let, b: Let): boolean =>
    ValueId.areEqual(a.let, b.let) &&
    Value.areEqual(a.be, b.be) &&
    Contract.areEqual(a.then, b.then);
}

export function Assert(assertObs: Observation, then: Contract): Assert {
  return { assert: assertObs, then };
}
export namespace Assert {
  export const jsonCodec: JsonCodec<Assert> = objectOf({
    assert: Observation.jsonCodec,
    then: Contract.jsonCodec,
  });
  export const areEqual = (a: Assert, b: Assert): boolean =>
    Observation.areEqual(a.assert, b.assert) && Contract.areEqual(a.then, b.then);
}

export namespace Timeout {
  export const jsonCodec: JsonCodec<Timeout> = json2BigIntCodec;
  // `Timeout` is currently a `bigint` alias. The helper is wired up so
  // that moving to a tagged/branded representation later only requires
  // swapping the body here.
  export const areEqual = (a: Timeout, b: Timeout): boolean => a === b;
}

export function datetoTimeout(date: Date): Timeout {
  return BigInt(Math.floor(date.getTime() / 1000) * 1000);
}

export function timeoutToDate(timeout: Timeout): Date {
  return new Date(Number(timeout));
}

export namespace NormalCase {
  export const jsonCodec: JsonCodec<NormalCase> = objectOf({
    case: Action.jsonCodec,
    then: Contract.jsonCodec,
  });
  export const areEqual = (a: NormalCase, b: NormalCase): boolean =>
    Action.areEqual(a.case, b.case) && Contract.areEqual(a.then, b.then);
}

export function MerkleizedCase(caseAction: Action, merkleized_then: string): Case {
  return { case: caseAction, merkleized_then };
}
export namespace MerkleizedCase {
  export const jsonCodec: JsonCodec<MerkleizedCase> = objectOf({
    case: Action.jsonCodec,
    merkleized_then: json2StringCodec,
  });
  export const areEqual = (a: MerkleizedCase, b: MerkleizedCase): boolean =>
    Action.areEqual(a.case, b.case) && a.merkleized_then === b.merkleized_then;
}

export function Case(caseAction: Action, continuation: Contract): Case {
  return { case: caseAction, then: continuation };
}
export namespace Case {
  export const isNormalCase = (c: Case): c is NormalCase => "then" in c;
  export const isMerkleizedCase = (c: Case): c is MerkleizedCase => "merkleized_then" in c;
  export const match = <T>(c: Case, handlers: { normal: (normalCase: NormalCase) => T, merkleized: (merkleizedCase: MerkleizedCase) => T }): T =>
    isNormalCase(c) ? handlers.normal(c) : handlers.merkleized(c);

  export const jsonCodec: JsonCodec<Case> = altJsonCodecs(
    [NormalCase.jsonCodec, MerkleizedCase.jsonCodec],
    (serNormal, serMerkleized) => (c: Case) =>
      isNormalCase(c) ? serNormal(c) : serMerkleized(c)
  );
  export const areEqual = (a: Case, b: Case): boolean =>
    isNormalCase(a)
      ? isNormalCase(b) && NormalCase.areEqual(a, b)
      : isMerkleizedCase(b) && MerkleizedCase.areEqual(a, b);
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
  export const areEqual = (a: When, b: When): boolean =>
    arrayAreEqualWith(a.when, b.when, Case.areEqual) &&
    Timeout.areEqual(a.timeout, b.timeout) &&
    Contract.areEqual(a.timeout_continuation, b.timeout_continuation);
}
