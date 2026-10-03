import { describe, expect, it } from "vitest";
import { parse, stringify, type Json } from "@konduit/codec/json";
import { unwrapOrPanic } from "@konduit/codec/neverthrow";
import {
  AccountId,
  Action,
  Address,
  AvailableMoney,
  Bound,
  Choice,
  ChoiceId,
  ChoiceName,
  ChosenNum,
  Constant,
  Deposit,
  Notify,
  Party,
  Payee,
  PayeeAccount,
  PayeeParty,
  PolicyId,
  Role,
  RoleName,
  Token,
  TokenName,
  Value,
  ValueId,
} from "./index.js";
import type { JsonCodec, JsonError } from "@konduit/codec/json/codecs";
import type { Result } from "neverthrow";


function roundTrip<T>(codec: JsonCodec<T>, value: T): T {
  const encoded = codec.serialise(value);
  const jsonStr = stringify(encoded);
  const json = unwrapOrPanic(parse(jsonStr), `parse failed: ${jsonStr}`);
  const decoded = unwrapOrPanic(codec.deserialise(json), `deserialise failed: ${stringify(json)}`);
  return decoded;
}

describe("Action codecs", () => {
  describe("Deposit", () => {
    it("round-trips a deposit with role parties", () => {
      const deposit: Deposit = {
        party: { role_token: "Bob" },
        deposits: 1000000n,
        of_token: { currency_symbol: "", token_name: "" },
        into_account: { role_token: "Alice" },
      };
      const result = roundTrip(Action.jsonCodec, deposit);
      expect(result).toEqual(deposit);
    });

    it("round-trips a deposit with address parties", () => {
      const deposit: Deposit = {
        party: { address: "addr_test1vr4tkl87dqa0m0hd4xp7h2kcxpcnj98dma40lh8das0xyss8hrgvs" },
        deposits: 1000000n,
        of_token: { currency_symbol: "abcd", token_name: "TKN" },
        into_account: { address: "addr_test1vr4tkl87dqa0m0hd4xp7h2kcxpcnj98dma40lh8das0xyss8hrgvs" },
      };
      const result = roundTrip(Action.jsonCodec, deposit);
      expect(result).toEqual(deposit);
    });

    it("round-trips a deposit with nested Value (NegValue)", () => {
      const deposit: Deposit = {
        party: { role_token: "Bob" },
        deposits: { negate: 500000n },
        of_token: { currency_symbol: "", token_name: "" },
        into_account: { role_token: "Alice" },
      };
      const result = roundTrip(Action.jsonCodec, deposit);
      expect(result).toEqual(deposit);
    });
  });

  describe("Choice", () => {
    it("round-trips a choice with multiple bounds", () => {
      const choice: Choice = {
        choose_between: [
          { from: 1n, to: 5n },
          { from: 10n, to: 20n },
        ],
        for_choice: {
          choice_name: "option",
          choice_owner: { role_token: "Bob" },
        },
      };
      const result = roundTrip(Action.jsonCodec, choice);
      expect(result).toEqual(choice);
    });

    it("round-trips a choice with empty bounds array", () => {
      const choice: Choice = {
        choose_between: [],
        for_choice: {
          choice_name: "none",
          choice_owner: { address: "addr_test1abc" },
        },
      };
      const result = roundTrip(Action.jsonCodec, choice);
      expect(result).toEqual(choice);
    });
  });

  describe("Notify", () => {
    it("round-trips a notify with boolean observation", () => {
      const notify: Notify = { notify_if: true };
      const result = roundTrip(Action.jsonCodec, notify);
      expect(result).toEqual(notify);
    });

    it("round-trips a notify with a complex observation", () => {
      const notify: Notify = {
        notify_if: {
          value: 1n,
          equal_to: 1n,
        },
      };
      const result = roundTrip(Action.jsonCodec, notify);
      expect(result).toEqual(notify);
    });

    it("round-trips a notify with nested ValueEQ observation referencing a Cond", () => {
      const notify: Notify = {
        notify_if: {
          value: {
            if: true,
            then: 10n,
            else: 20n,
          },
          equal_to: 10n,
        },
      };
      const result = roundTrip(Action.jsonCodec, notify);
      expect(result).toEqual(notify);
    });
  });

  describe("Union dispatch", () => {
    it("serialises Deposit via the deposit serialiser", () => {
      const deposit: Deposit = {
        party: { role_token: "Bob" },
        deposits: 1n,
        of_token: { currency_symbol: "", token_name: "" },
        into_account: { role_token: "Alice" },
      };
      const encoded = Action.jsonCodec.serialise(deposit);
      expect(encoded).toEqual(deposit);
    });

    it("serialises Choice via the choice serialiser", () => {
      const choice: Choice = {
        choose_between: [{ from: 0n, to: 1n }],
        for_choice: { choice_name: "x", choice_owner: { role_token: "Bob" } },
      };
      const encoded = Action.jsonCodec.serialise(choice);
      expect(encoded).toEqual(choice);
    });

    it("serialises Notify via the notify serialiser", () => {
      const notify: Notify = { notify_if: false };
      const encoded = Action.jsonCodec.serialise(notify);
      expect(encoded).toEqual(notify);
    });
  });

  describe("parse round-trip preserves bigint", () => {
    it("keeps a bigint Constant intact through JSON", () => {
      const deposit: Deposit = {
        party: { role_token: "Bob" },
        deposits: 123456789012345678901234567890n,
        of_token: { currency_symbol: "", token_name: "" },
        into_account: { role_token: "Alice" },
      };
      const jsonStr = stringify(Action.jsonCodec.serialise(deposit));
      const parsed = parse(jsonStr);
      if (parsed.isErr()) throw new Error(parsed.error);
      const decoded = Action.jsonCodec.deserialise(parsed.value);
      if (decoded.isErr()) throw new Error(JSON.stringify(decoded.error));
      expect(decoded.value).toEqual(deposit);
    });
  });
});

describe("Dependent codecs smoke test", () => {
  it("Party jsonCodec round-trips both variants", () => {
    expect(roundTrip(Party.jsonCodec, { role_token: "Alice" })).toEqual({
      role_token: "Alice",
    });
    expect(
      roundTrip(Party.jsonCodec, { address: "addr_test1abc" })
    ).toEqual({ address: "addr_test1abc" });
  });

  it("Token jsonCodec round-trips", () => {
    expect(roundTrip(Token.jsonCodec, { currency_symbol: "abcd", token_name: "TKN" })).toEqual({
      currency_symbol: "abcd",
      token_name: "TKN",
    });
  });

  it("ChoiceId jsonCodec round-trips", () => {
    expect(
      roundTrip(ChoiceId.jsonCodec, {
        choice_name: "option",
        choice_owner: { role_token: "Bob" },
      })
    ).toEqual({
      choice_name: "option",
      choice_owner: { role_token: "Bob" },
    });
  });

  it("Bound jsonCodec round-trips", () => {
    expect(roundTrip(Bound.jsonCodec, { from: 1n, to: 10n })).toEqual({ from: 1n, to: 10n });
  });

  it("AccountId jsonCodec round-trips", () => {
    expect(roundTrip(AccountId.jsonCodec, { role_token: "Alice" })).toEqual({
      role_token: "Alice",
    });
  });

  it("Payee jsonCodec round-trips account and party variants", () => {
    expect(
      roundTrip(Payee.jsonCodec, { account: { role_token: "Alice" } })
    ).toEqual({ account: { role_token: "Alice" } });
    expect(
      roundTrip(Payee.jsonCodec, { party: { role_token: "Bob" } })
    ).toEqual({ party: { role_token: "Bob" } });
  });

  it("Value jsonCodec round-trips a deeply nested expression", () => {
    const v: Value = {
      if: { both: true, and: false },
      then: { negate: 10n },
      else: "time_interval_end",
    };
    const result = roundTrip(Value.jsonCodec, v);
    expect(result).toEqual(v);
  });
});
