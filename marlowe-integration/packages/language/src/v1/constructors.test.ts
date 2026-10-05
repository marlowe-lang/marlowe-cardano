import { describe, expect, it } from "vitest";
import { parse, stringify } from "@konduit/codec/json";
import { unwrapOrPanic } from "@konduit/codec/neverthrow";
import type { JsonCodec } from "@konduit/codec/json/codecs";
import {
  AmbiguousTimeIntervalError,
  ApplyNoMatchError,
  AssertionFailed,
  BuiltinByteString,
  Close,
  Contract,
  HashMismatchError,
  IDeposit,
  IChoice,
  INotify,
  InvalidInterval,
  IntervalInPast,
  MerkleizedChoice,
  MerkleizedDeposit,
  MerkleizedHashAndContinuation,
  MerkleizedNotify,
  NonPositiveDeposit,
  NonPositivePay,
  PartialPay,
  Party,
  Payment,
  Payee,
  PolicyId,
  Role,
  RoleName,
  TEIntervalError,
  Address,
  Token,
  Transaction,
  TransactionErrorCase,
  TransactionSuccess,
  UselessTransaction,
  Choice,
  ChoiceId,
  ChoiceName,
  ChosenNum,
  Bound,
  AccountId,
  adaToken,
} from "./index.js";

function roundTrip<T>(codec: JsonCodec<T>, value: T): T {
  const encoded = codec.serialise(value);
  const jsonStr = stringify(encoded);
  const json = unwrapOrPanic(parse(jsonStr), `parse failed: ${jsonStr}`);
  return unwrapOrPanic(codec.deserialise(json), `deserialise failed: ${stringify(json)}`);
}

const ada: Token = adaToken;
const party: Party = Party(Address("addr_test1abc"));
const close: Contract = Close();

describe("constructor smoke test", () => {
  it("IDeposit / IChoice / INotify / BuiltinByteString construct and round-trip", () => {
    const d = IDeposit(party, 1000n, ada, party);
    expect(roundTrip(IDeposit.jsonCodec, d)).toEqual(d);

    const c = IChoice(ChoiceId(ChoiceName("x"), party), ChosenNum(5n));
    expect(roundTrip(IChoice.jsonCodec, c)).toEqual(c);

    expect(INotify()).toBe("input_notify");
    expect(BuiltinByteString("deadbeef")).toBe("deadbeef");
    expect(PolicyId("")).toBe("");
    expect(RoleName("Alice")).toBe("Alice");
    expect(AccountId(party)).toBe(party);
  });

  it("Merkleized* constructors build valid values", () => {
    const h = BuiltinByteString("h");
    expect(MerkleizedHashAndContinuation(h, close)).toEqual({ continuation_hash: h, merkleized_continuation: close });
    expect(MerkleizedDeposit(party, 1n, ada, party, h, close).continuation_hash).toBe(h);
    expect(MerkleizedChoice(ChoiceId(ChoiceName("x"), party), 5n, h, close).merkleized_continuation).toBe(close);
    expect(MerkleizedNotify(h, close)).toEqual({ continuation_hash: h, merkleized_continuation: close });
  });

  it("Payment / Transaction construct", () => {
    const p: ReturnType<typeof Payment> = Payment(party, Payee(party), 1n, ada);
    expect(p.amount).toBe(1n);
    const t: ReturnType<typeof Transaction> = Transaction({ from: 0n, to: 1n }, []);
    expect(t.tx_inputs).toEqual([]);
  });

  it("NonPositiveDeposit / NonPositivePay / PartialPay construct", () => {
    expect(NonPositiveDeposit(party, 0n, ada, party).asked_to_deposit).toBe(0n);
    expect(NonPositivePay(party, 0n, ada, Payee(party)).asked_to_pay).toBe(0n);
    expect(PartialPay(party, 0n, ada, Payee(party), 0n).but_only_paid).toBe(0n);
  });

  it("AssertionFailed and TE error constants construct", () => {
    expect(AssertionFailed()).toBe("assertion_failed");
    expect(AmbiguousTimeIntervalError()).toBe("TEAmbiguousTimeIntervalError");
    expect(ApplyNoMatchError()).toBe("TEApplyNoMatchError");
    expect(UselessTransaction()).toBe("TEUselessTransaction");
    expect(HashMismatchError()).toBe("TEHashMismatch");
  });

  it("InvalidInterval / IntervalInPast / TEIntervalError construct", () => {
    const ii: ReturnType<typeof InvalidInterval> = InvalidInterval({ from: 0n, to: 1n });
    expect(ii.invalidInterval.from).toBe(0n);
    const ip: ReturnType<typeof IntervalInPast> = IntervalInPast({ from: 0n, to: 1n, minTime: 0n });
    expect(ip.intervalInPastError.minTime).toBe(0n);
    const te: ReturnType<typeof TEIntervalError> = TEIntervalError(InvalidInterval({ from: 0n, to: 1n }));
    expect(te.error).toBe("TEIntervalError");
  });

  it("TransactionSuccess / TransactionErrorCase construct", () => {
    const success: ReturnType<typeof TransactionSuccess> = TransactionSuccess([], [], { accounts: [], boundValues: [], choices: [], minTime: 0n }, close);
    expect(success.contract).toBe(close);
    const error: ReturnType<typeof TransactionErrorCase> = TransactionErrorCase(AmbiguousTimeIntervalError());
    expect(error.transaction_error).toBe("TEAmbiguousTimeIntervalError");
  });
});
