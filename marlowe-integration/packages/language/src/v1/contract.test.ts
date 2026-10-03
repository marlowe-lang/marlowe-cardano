import { describe, expect, it } from "vitest";
import { parse, stringify } from "@konduit/codec/json";
import { unwrapOrPanic } from "@konduit/codec/neverthrow";
import type { JsonCodec } from "@konduit/codec/json/codecs";
import {
  Close,
  Contract,
  IChoice,
  IDeposit,
  INotify,
  Input,
  InputContent,
  MarloweState,
  Pay,
  Token,
} from "./index.js";

function roundTrip<T>(codec: JsonCodec<T>, value: T): T {
  const encoded = codec.serialise(value);
  const jsonStr = stringify(encoded);
  const json = unwrapOrPanic(parse(jsonStr), `parse failed: ${jsonStr}`);
  const decoded = unwrapOrPanic(codec.deserialise(json), `deserialise failed: ${stringify(json)}`);
  return decoded;
}

const ada: Token = { currency_symbol: "", token_name: "" };

describe("Contract codecs", () => {
  describe("Close", () => {
    it("round-trips", () => {
      expect(roundTrip(Close.jsonCodec, "close")).toBe("close");
    });
  });

  describe("Pay + Contract", () => {
    it("round-trips a Pay whose `then` is close", () => {
      const pay: Pay = {
        pay: 1000000n,
        token: ada,
        from_account: { role_token: "Alice" },
        to: { party: { role_token: "Bob" } },
        then: "close",
      };
      expect(roundTrip(Contract.jsonCodec, pay)).toEqual(pay);
    });

    it("round-trips a deeply nested Contract through When → Pay → close", () => {
      const inner: Pay = {
        pay: 1n,
        token: ada,
        from_account: { role_token: "A" },
        to: { account: { role_token: "B" } },
        then: "close",
      };
      const contract: Contract = {
        when: [
          {
            case: {
              party: { role_token: "C" },
              deposits: 1n,
              of_token: ada,
              into_account: { role_token: "D" },
            },
            then: inner,
          },
        ],
        timeout: 1000n,
        timeout_continuation: "close",
      };
      expect(roundTrip(Contract.jsonCodec, contract)).toEqual(contract);
    });
  });
});

describe("Input codecs", () => {
  it("round-trips an IDeposit", () => {
    const dep: IDeposit = {
      input_from_party: { role_token: "Bob" },
      that_deposits: 1000000n,
      of_token: ada,
      into_account: { role_token: "Alice" },
    };
    expect(roundTrip(IDeposit.jsonCodec, dep)).toEqual(dep);
  });

  it("round-trips an IChoice", () => {
    const ch: IChoice = {
      for_choice_id: {
        choice_name: "option",
        choice_owner: { role_token: "Bob" },
      },
      input_that_chooses_num: 5n,
    };
    expect(roundTrip(IChoice.jsonCodec, ch)).toEqual(ch);
  });

  it("round-trips an INotify (string constant)", () => {
    expect(roundTrip(INotify.jsonCodec, "input_notify")).toBe("input_notify");
  });

  it("round-trips an InputContent union", () => {
    const inputs: InputContent[] = [
      {
        input_from_party: { role_token: "Bob" },
        that_deposits: 1n,
        of_token: ada,
        into_account: { role_token: "Alice" },
      },
      {
        for_choice_id: {
          choice_name: "x",
          choice_owner: { role_token: "Bob" },
        },
        input_that_chooses_num: 1n,
      },
      "input_notify",
    ];
    for (const input of inputs) {
      expect(roundTrip(InputContent.jsonCodec, input)).toEqual(input);
    }
  });

  it("round-trips an Input union (NormalInput | MerkleizedInput)", () => {
    const inputs: Input[] = [
      {
        input_from_party: { role_token: "Bob" },
        that_deposits: 1n,
        of_token: ada,
        into_account: { role_token: "Alice" },
      },
      {
        for_choice_id: {
          choice_name: "x",
          choice_owner: { role_token: "Bob" },
        },
        input_that_chooses_num: 1n,
      },
      "input_notify",
    ];
    for (const input of inputs) {
      expect(roundTrip(Input.jsonCodec, input)).toEqual(input);
    }
  });
});

describe("MarloweState", () => {
  it("round-trips a minimal state", () => {
    const state: MarloweState = {
      accounts: [],
      boundValues: [],
      choices: [],
      minTime: 0n,
    };
    expect(roundTrip(MarloweState.jsonCodec, state)).toEqual(state);
  });

  it("round-trips a state with accounts and choices", () => {
    const state: MarloweState = {
      accounts: [
        [[{ role_token: "Alice" }, ada], 1000000n],
      ],
      boundValues: [["v1", 42n]],
      choices: [
        [
          {
            choice_name: "opt",
            choice_owner: { role_token: "Bob" },
          },
          7n,
        ],
      ],
      minTime: 100n,
    };
    expect(roundTrip(MarloweState.jsonCodec, state)).toEqual(state);
  });
});