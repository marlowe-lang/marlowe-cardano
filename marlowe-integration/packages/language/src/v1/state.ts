import {
  arrayOf,
  json2BigIntCodec,
  objectOf,
  tupleOf,
  type JsonCodec,
} from "@konduit/codec/json/codecs";
import { arrayAreEqualWith, type AssocMap } from "../assoc-map.js";
import { AccountId } from "./payee.js";
import { ValueId } from "./value-and-observation.js";
import { Token } from "./token.js";
import { ChoiceId } from "./choices.js";
import { Party } from "./participants.js";

// An `AssocMap<K, V>` is encoded as an array of `[K, V]` pairs. For
// heterogeneous pairs we use nested `tupleOf` to model the JSON shape
// `[ [K1, K2], V ]`.
export type Accounts = AssocMap<[AccountId, Token], bigint>;

type Sort = "GreaterThan" | "LowerThan" | "EqualTo";

export function accountsCmp(a: [AccountId, Token], b: [AccountId, Token]): Sort {
  const accIdCmp = partyCmp(a[0], b[0]);
  if (accIdCmp !== "EqualTo") {
    return accIdCmp;
  }
  return tokenCmp(a[1], b[1]);
}

const accountKeyAreEqual = (
  a: [AccountId, Token],
  b: [AccountId, Token],
): boolean => AccountId.areEqual(a[0], b[0]) && Token.areEqual(a[1], b[1]);

const assocMapAreEqual = <K, V>(
  a: AssocMap<K, V>,
  b: AssocMap<K, V>,
  eq: (x: K, y: K) => boolean,
): boolean => arrayAreEqualWith(a, b, (ae, be) => eq(ae[0], be[0]) && ae[1] === be[1]);

export namespace Accounts {
  export const jsonCodec: JsonCodec<Accounts> = arrayOf(
    tupleOf(
      tupleOf(AccountId.jsonCodec, Token.jsonCodec),
      json2BigIntCodec
    )
  );
  export const areEqual = (a: Accounts, b: Accounts): boolean =>
    assocMapAreEqual(a, b, accountKeyAreEqual);
}

export type MarloweState = {
  accounts: Accounts;
  boundValues: AssocMap<ValueId, bigint>;
  choices: AssocMap<ChoiceId, bigint>;
  minTime: bigint;
};

export namespace MarloweState {
  export const jsonCodec: JsonCodec<MarloweState> = objectOf({
    accounts: Accounts.jsonCodec,
    boundValues: arrayOf(tupleOf(ValueId.jsonCodec, json2BigIntCodec)),
    choices: arrayOf(tupleOf(ChoiceId.jsonCodec, json2BigIntCodec)),
    minTime: json2BigIntCodec,
  });
  export const areEqual = (a: MarloweState, b: MarloweState): boolean =>
    Accounts.areEqual(a.accounts, b.accounts) &&
    assocMapAreEqual(a.boundValues, b.boundValues, ValueId.areEqual) &&
    assocMapAreEqual(a.choices, b.choices, ChoiceId.areEqual) &&
    a.minTime === b.minTime;
}

function partyCmp(a: Party, b: Party): Sort {
  if ("role_token" in a && !("role_token" in b)) {
    return "LowerThan";
  }
  if (!("role_token" in a) && "role_token" in b) {
    return "GreaterThan";
  }
  if ("address" in a && "address" in b) {
    return strCmp(a.address, b.address);
  }
  if ("role_token" in a && "role_token" in b) {
    return strCmp(a.role_token, b.role_token);
  }
  throw new Error("Unreachable");
}

function strCmp(a: string, b: string): Sort {
  if (a < b) {
    return "LowerThan";
  }
  if (a > b) {
    return "GreaterThan";
  }
  return "EqualTo";
}

function tokenCmp(a: Token, b: Token): Sort {
  const currencyCmp = strCmp(a.currency_symbol, b.currency_symbol);
  if (currencyCmp !== "EqualTo") {
    return currencyCmp;
  }
  return strCmp(a.token_name, b.token_name);
}