import {
  arrayOf,
  json2BigIntCodec,
  objectOf,
  tupleOf,
  type JsonCodec,
} from "@konduit/codec/json/codecs";
import type { AssocMap } from "../assoc-map.js";
import type { AccountId } from "./payee.js";
import { AccountId as AccountIdNamespace } from "./payee.js";
import type { ValueId } from "./value-and-observation.js";
import { ValueId as ValueIdNamespace } from "./value-and-observation.js";
import type { Token } from "./token.js";
import { Token as TokenNamespace } from "./token.js";
import type { ChoiceId } from "./choices.js";
import { ChoiceId as ChoiceIdNamespace } from "./choices.js";
import type { Party } from "./participants.js";
import { Party as PartyNamespace } from "./participants.js";

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

export namespace Accounts {
  export const jsonCodec: JsonCodec<Accounts> = arrayOf(
    tupleOf(
      tupleOf(AccountIdNamespace.jsonCodec, TokenNamespace.jsonCodec),
      json2BigIntCodec
    )
  );
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
    boundValues: arrayOf(tupleOf(ValueIdNamespace.jsonCodec, json2BigIntCodec)),
    choices: arrayOf(tupleOf(ChoiceIdNamespace.jsonCodec, json2BigIntCodec)),
    minTime: json2BigIntCodec,
  });
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