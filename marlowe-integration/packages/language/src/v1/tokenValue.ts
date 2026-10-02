import {
  json2BigIntCodec,
  objectOf,
  type JsonCodec,
} from "@konduit/codec/json/codecs";
import type { Token } from "./token.js";
import { Token as TokenNamespace } from "./token.js";

export type TokenValue = {
  amount: bigint;
  token: Token;
};

export function TokenValue(amount: bigint, token: Token): TokenValue {
  return { amount, token };
}
export namespace TokenValue {
  export const jsonCodec: JsonCodec<TokenValue> = objectOf({
    amount: json2BigIntCodec,
    token: TokenNamespace.jsonCodec,
  });
}

// Helpers --------------------------------------------------------------------

export const tokenValue: (amount: bigint) => (token: Token) => TokenValue =
  (amount) => (token) => TokenValue(amount, token);

export const lovelaceValue: (lovelaces: bigint) => TokenValue = (lovelaces) =>
  TokenValue(lovelaces, TokenNamespace("", ""));

export const adaValue: (adaAmount: bigint) => TokenValue = (adaAmount) =>
  TokenValue(adaAmount * 1_000_000n, TokenNamespace("", ""));