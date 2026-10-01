import type { Sort } from "../assoc-map.js";
import { json2StringCodec, objectOf, type JsonCodec } from "@konduit/codec/json/codecs";
import type { PolicyId } from "./policyId.js";
import { PolicyId as PolicyIdNamespace } from "./policyId.js";

export type TokenName = string;

export namespace TokenName {
  export const jsonCodec: JsonCodec<TokenName> = json2StringCodec;
}

export interface Token {
  currency_symbol: PolicyId;
  token_name: TokenName;
}

export const token = (currency_symbol: PolicyId, token_name: TokenName) => ({
  currency_symbol: currency_symbol,
  token_name: token_name,
});

export const tokenToString: (token: Token) => string = (token) => `${token.currency_symbol}|${token.token_name}`;

export const lovelace: Token = token("", "");

export const adaToken: Token = lovelace;

export namespace Token {
  export const jsonCodec: JsonCodec<Token> = objectOf({
    currency_symbol: PolicyIdNamespace.jsonCodec,
    token_name: TokenName.jsonCodec,
  });
}

export function tokenCmp(a: Token, b: Token): Sort {
  const currencyCmp = strCmp(a.currency_symbol, b.currency_symbol);
  if (currencyCmp !== "EqualTo") {
    return currencyCmp;
  }
  return strCmp(a.token_name, b.token_name);
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
