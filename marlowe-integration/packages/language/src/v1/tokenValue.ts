import {
  json2BigIntCodec,
  objectOf,
  type JsonCodec,
} from "@konduit/codec/json/codecs";
import { Token } from "./token.js";

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
    token: Token.jsonCodec,
  });
  export const areEqual = (a: TokenValue, b: TokenValue): boolean =>
    a.amount === b.amount && Token.areEqual(a.token, b.token);
}

// Helpers --------------------------------------------------------------------

export const tokenValue: (amount: bigint) => (token: Token) => TokenValue =
  (amount) => (token) => TokenValue(amount, token);

export const lovelaceValue: (lovelaces: bigint) => TokenValue = (lovelaces) =>
  TokenValue(lovelaces, Token("", ""));

export const adaValue: (adaAmount: bigint) => TokenValue = (adaAmount) =>
  TokenValue(adaAmount * 1_000_000n, Token("", ""));