import { describe, expect, it } from "vitest";
import {
  AddValue,
  BuiltinByteString,
  Case,
  ChoiceName,
  ChosenNum,
  Close,
  Contract,
  Deposit,
  MarloweState,
  Party,
  PolicyId,
  RoleName,
  TimeInterval,
  TokenName,
  Transaction,
  Value,
  ValueId,
  When,
  lovelace,
} from "./index.js";

describe("areEqual smoke test", () => {
  it("primitive aliases compare with === semantics", () => {
    expect(PolicyId.areEqual("a", "a")).toBe(true);
    expect(PolicyId.areEqual("a", "b")).toBe(false);
    expect(TokenName.areEqual("a", "a")).toBe(true);
    expect(ChoiceName.areEqual("a", "b")).toBe(false);
    expect(RoleName.areEqual("a", "a")).toBe(true);
    expect(ChosenNum.areEqual(1n, 1n)).toBe(true);
    expect(ChosenNum.areEqual(1n, 2n)).toBe(false);
    expect(ValueId.areEqual("x", "x")).toBe(true);
    expect(BuiltinByteString.areEqual("x", "y")).toBe(false);
  });

  it("Value.areEqual recurses through nested arithmetic", () => {
    const a: Value = AddValue(1n, 2n);
    const b: Value = AddValue(1n, 2n);
    const c: Value = AddValue(1n, 3n);
    expect(Value.areEqual(a, b)).toBe(true);
    expect(Value.areEqual(a, c)).toBe(false);
  });

  it("Party.areEqual covers address and role variants", () => {
    const a = Party({ address: "addr_test1abc" });
    const b = Party({ address: "addr_test1abc" });
    const c = Party({ address: "addr_test1xyz" });
    const r = Party({ role_token: "Bob" });
    const r2 = Party({ role_token: "Bob" });
    expect(Party.areEqual(a, b)).toBe(true);
    expect(Party.areEqual(a, c)).toBe(false);
    expect(Party.areEqual(r, r2)).toBe(true);
    expect(Party.areEqual(a, r)).toBe(false);
  });

  it("Contract.areEqual recurses through When / Close / Pay / If / Let / Assert", () => {
    const p1 = Party({ address: "addr_test1p1" });
    const p2 = Party({ address: "addr_test1p2" });
    const c1: Contract = When(
      [Case(Deposit(p1, p1, lovelace, 1000n), Close())],
      1000n,
      Close(),
    );
    const c2: Contract = When(
      [Case(Deposit(p1, p1, lovelace, 1000n), Close())],
      1000n,
      Close(),
    );
    const c3: Contract = When(
      [Case(Deposit(p1, p1, lovelace, 2000n), Close())],
      1000n,
      Close(),
    );
    expect(Contract.areEqual(c1, c2)).toBe(true);
    expect(Contract.areEqual(c1, c3)).toBe(false);
    expect(Contract.areEqual(Close(), Close())).toBe(true);
  });

  it("MarloweState.areEqual compares all four fields", () => {
    const a: MarloweState = {
      accounts: [],
      boundValues: [],
      choices: [],
      minTime: 0n,
    };
    const b: MarloweState = {
      accounts: [],
      boundValues: [],
      choices: [],
      minTime: 0n,
    };
    const c: MarloweState = {
      accounts: [],
      boundValues: [],
      choices: [],
      minTime: 1n,
    };
    expect(MarloweState.areEqual(a, b)).toBe(true);
    expect(MarloweState.areEqual(a, c)).toBe(false);
  });

  it("Transaction.areEqual compares the interval and the input list", () => {
    const tx1: Transaction = {
      tx_interval: TimeInterval(0n, 1000n),
      tx_inputs: [],
    };
    const tx2: Transaction = {
      tx_interval: TimeInterval(0n, 1000n),
      tx_inputs: [],
    };
    const tx3: Transaction = {
      tx_interval: TimeInterval(0n, 1000n),
      tx_inputs: ["input_notify"],
    };
    expect(Transaction.areEqual(tx1, tx2)).toBe(true);
    expect(Transaction.areEqual(tx1, tx3)).toBe(false);
  });
});
