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
import { err, ok } from "neverthrow";
import type { Result } from "neverthrow";
import { areEqualThunk, arrayAreEqualWith } from "../assoc-map.js";
import { Observation, Value, ValueId } from "./value-and-observation.js";
import { AccountId, Payee } from "./payee.js";
import { Token } from "./token.js";
import { Action, Choice, Deposit, Notify } from "./actions.js";

export type Contract = Assert | Close | If | Let | Pay | When;
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

  export const tryMatch = <T>(contract: Contract, handlers: { assert?: (assert: Assert) => T, close?: (close: Close) => T, if?: (if_: If) => T, let?: (let_: Let) => T, pay?: (pay: Pay) => T, when?: (when: When) => T }): Result<T, string> =>
    isAssert(contract) ? handlers.assert ? ok(handlers.assert(contract)) : err("Missing assert handler")
    : isClose(contract) ? handlers.close ? ok(handlers.close(contract)) : err("Missing close handler")
    : isIf(contract) ? handlers.if ? ok(handlers.if(contract)) : err("Missing if handler")
    : isLet(contract) ? handlers.let ? ok(handlers.let(contract)) : err("Missing let handler")
    : isPay(contract) ? handlers.pay ? ok(handlers.pay(contract)) : err("Missing pay handler")
    : handlers.when ? ok(handlers.when(contract)) : err("Missing when handler")

  export const tryMatchAndThen = <T>(contract: Contract, handlers: { assert?: (assert: Assert) => Result<T, string>, close?: (close: Close) => Result<T, string>, if?: (if_: If) => Result<T, string>, let?: (let_: Let) => Result<T, string>, pay?: (pay: Pay) => Result<T, string>, when?: (when: When) => Result<T, string> }): Result<T, string> =>
    tryMatch(contract, handlers).andThen(result => result);

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

export type Close = "close";

export function Close(): Close {
  return "close";
}
export namespace Close {
  export const jsonCodec: JsonCodec<Close> = jsonConstant("close");
  export const areEqual = (a: Close, b: Close): boolean => a === b;
}

export type Pay = {
  pay: Value;
  token: Token;
  from_account: AccountId;
  to: Payee;
  then: Contract;
};
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

export type If = {
  if: Observation;
  then: Contract;
  else: Contract;
};
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

export type Let = {
  let: ValueId;
  be: Value;
  then: Contract;
};
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

export type Assert = {
  assert: Observation;
  then: Contract;
};
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

export type NormalCase = {
  case: Action;
  then: Contract;
};

export function NormalCase(caseAction: Action, then: Contract): NormalCase {
  return { case: caseAction, then };
}

export namespace NormalCase {
  export const jsonCodec: JsonCodec<NormalCase> = objectOf({
    case: Action.jsonCodec,
    then: Contract.jsonCodec,
  });

  export const areEqual = (a: NormalCase, b: NormalCase): boolean =>
    Action.areEqual(a.case, b.case) && Contract.areEqual(a.then, b.then);
}

export type MerkleizedCase = {
  case: Action;
  merkleized_then: string;
};

export function MerkleizedCase(caseAction: Action, merkleized_then: string): MerkleizedCase {
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

export type Case = NormalCase | MerkleizedCase;

export function Case(caseAction: Action, continuation: Contract | string): Case {
  if (typeof continuation === "string" && continuation !== Close()) {
    return MerkleizedCase(caseAction, continuation);
  }
  return NormalCase(caseAction, continuation);
}

export namespace Case {
  export const isNormalCase = (c: Case): c is NormalCase => "then" in c;
  export const isMerkleizedCase = (c: Case): c is MerkleizedCase => "merkleized_then" in c;
  export const match = <T>(c: Case, handlers: { normal: (normalCase: NormalCase) => T, merkleized: (merkleizedCase: MerkleizedCase) => T }): T =>
    isNormalCase(c) ? handlers.normal(c) : handlers.merkleized(c);

  export const tryMatch = <T>(c: Case, handlers: { normal?: (normalCase: NormalCase) => T, merkleized?: (merkleizedCase: MerkleizedCase) => T }): Result<T, string> =>
    isNormalCase(c) ? handlers.normal ? ok(handlers.normal(c)) : err("Missing normal handler")
    : handlers.merkleized ? ok(handlers.merkleized(c)) : err("Missing merkleized handler");

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

export type Timeout = bigint;

export namespace Timeout {
  export const jsonCodec: JsonCodec<Timeout> = json2BigIntCodec;
  // `Timeout` is currently a `bigint` alias. The helper is wired up so
  // that moving to a tagged/branded representation later only requires
  // swapping the body here.
  export const areEqual = (a: Timeout, b: Timeout): boolean => a === b;
  export const fromDate = (date: Date): Timeout =>
    BigInt(Math.floor(date.getTime() / 1000) * 1000);
  export const toDate = (timeout: Timeout): Date => new Date(Number(timeout));
}

// When ------------------------------------------------------------------------

export type When = {
  when: Case[];
  timeout: Timeout;
  timeout_continuation: Contract;
};

export function When(cases: Case[], timeout: Timeout, timeoutCont?: Contract): When {
  return { when: cases, timeout, timeout_continuation: timeoutCont ?? Close() };
}

// Helper constructor for creating `When` contract with a single case.
// The order of arguments is chosen to make continuation nesting more ergonomic:
// ```
//   WhenAction(
//     Deposit(party1, party1, lovelace, amount),
//     timeout,
//     WhenAction(
//       Choice([{ from: NO_WINNERS, to: TEAM_2_WINS }], ChoiceId(CHOICE_NAME, oracle)),
//       timeout,
//       // continuation,
//     )
//    )
//  ```

export function WhenAction(action: Deposit | Choice | Notify, timeout: Timeout, continuation: Contract, timeoutCont?: Contract): When {
  return When(
    [Case(action, continuation)],
    timeout,
    timeoutCont ?? Close()
  );
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

  const id = <T>(x: T): T => x;

  // Matching over singleton When cases.
  export const matchNormalCase = (c: Contract): Result<[NormalCase, Timeout, Contract], JsonError> =>
    Contract.tryMatch(c, { when: id })
      .andThen(({ when: cases, timeout, timeout_continuation }) => {
        if (cases.length !== 1) {
          return err(`Expected exactly one case in the When, but got ${cases.length}`);
        }
        return Case.tryMatch(cases[0], { normal: id }).map(normalCase =>
          [normalCase, timeout, timeout_continuation] as [NormalCase, Timeout, Contract]
        );
      });

  export const matchDeposit = (c: Contract): Result<[Deposit, Contract, Timeout, Contract], JsonError> =>
    matchNormalCase(c)
      .andThen(([{ case: action, then }, t , tc]) => Action.tryMatch(action, { deposit: id })
        .map(deposit => [deposit, then, t, tc] as [Deposit, Contract, Timeout, Contract]));

  export const matchChoice = (c: Contract): Result<[Choice, Contract, Timeout, Contract], JsonError> =>
    matchNormalCase(c)
      .andThen(([{ case: action, then }, t, tc]) => Action.tryMatch(action, { choice: id })
        .map(choice => [choice, then, t, tc] as [Choice, Contract, Timeout, Contract]));

  export const matchNotify = (c: Contract): Result<[Notify, Contract, Timeout, Contract], JsonError> =>
    matchNormalCase(c)
      .andThen(([{ case: action, then }, t, tc]) => Action.tryMatch(action, { notify: id })
        .map(notify => [notify, then, t, tc] as [Notify, Contract, Timeout, Contract]));
}
