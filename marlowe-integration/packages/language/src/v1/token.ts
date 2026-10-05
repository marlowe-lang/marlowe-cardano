import type { Sort } from "../assoc-map.js";
import { json2StringCodec, objectOf, type JsonCodec } from "@konduit/codec/json/codecs";
import { PolicyId } from "./policyId.js";

export type TokenName = string;
export function TokenName(value: string): TokenName {
  return value;
}
export namespace TokenName {
  export const jsonCodec: JsonCodec<TokenName> = json2StringCodec;
  // `TokenName` is currently a `string` alias. The helper is wired up so
  // that moving to a tagged/branded representation later only requires
  // swapping the body here — all call sites already go through
  // `TokenName.areEqual`.
  export const areEqual = (a: TokenName, b: TokenName): boolean => a === b;
}

export type Token = {
  currency_symbol: PolicyId;
  token_name: TokenName;
};
export function Token(currency_symbol: PolicyId, token_name: TokenName): Token {
  return { currency_symbol, token_name };
}
export namespace Token {
  export const jsonCodec: JsonCodec<Token> = objectOf({
    currency_symbol: PolicyId.jsonCodec,
    token_name: TokenName.jsonCodec,
  });
  export const areEqual = (a: Token, b: Token): boolean =>
    PolicyId.areEqual(a.currency_symbol, b.currency_symbol) &&
    TokenName.areEqual(a.token_name, b.token_name);
}

export const tokenToString: (token: Token) => string = (token) =>
  `${token.currency_symbol}|${token.token_name}`;

export const lovelace: Token = Token("", "");

export const adaToken: Token = lovelace;

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