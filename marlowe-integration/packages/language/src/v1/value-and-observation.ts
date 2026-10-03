import {
  constant as jsonConstant,
  altJsonCodecs,
  json2BigIntCodec,
  json2BooleanCodec,
  json2StringCodec,
  objectOf,
  type JsonCodec,
} from "@konduit/codec/json/codecs";
import type { Json } from "@konduit/codec/json";
import { areEqualThunk } from "../assoc-map.js";
import { ChoiceId } from "./choices.js";
import { AccountId } from "./payee.js";
import { Token } from "./token.js";
import { fromCodecThunkFn } from "@konduit/codec";

// Namespaces are sensitive to the order of declaration, so the `Value` and
// `Observation` types and namespaces are declared first so that their
// subsequent references are valid.


// Namespaces are sensitive to the order of declaration, so I'm putting
// the `Value` and `Observation` types and namespaces first so their
// subsequent references are valid.

// Core `Value` type ----------------------------------------------------------------

export type Value =
  | AvailableMoney
  | Constant
  | NegValue
  | AddValue
  | SubValue
  | MulValue
  | DivValue
  | ChoiceValue
  | TimeIntervalStart
  | TimeIntervalEnd
  | UseValue
  | Cond;

export namespace Value {
  // We construct this codec lazily because its variants codecs reference it forming a cycle.
  export const jsonCodec: JsonCodec<Value> = fromCodecThunkFn(() => altJsonCodecs(
    [
      AvailableMoney.jsonCodec,
      Constant.jsonCodec,
      NegValue.jsonCodec,
      AddValue.jsonCodec,
      SubValue.jsonCodec,
      MulValue.jsonCodec,
      DivValue.jsonCodec,
      ChoiceValue.jsonCodec,
      TimeIntervalStart.jsonCodec,
      TimeIntervalEnd.jsonCodec,
      UseValue.jsonCodec,
      Cond.jsonCodec,
    ],
    (
      serAvail,
      serConst,
      serNeg,
      serAdd,
      serSub,
      serMul,
      serDiv,
      serChoice,
      serStart,
      serEnd,
      serUse,
      serCond
    ) => (value: Value): Json => {
      if (typeof value === "bigint") return serConst(value);
      if (value === "time_interval_start") return serStart(value);
      if (value === "time_interval_end") return serEnd(value);
      if ("amount_of_token" in value) return serAvail(value);
      if ("negate" in value) return serNeg(value);
      if ("add" in value) return serAdd(value);
      if ("value" in value && "minus" in value) return serSub(value);
      if ("multiply" in value) return serMul(value);
      if ("divide" in value) return serDiv(value);
      if ("value_of_choice" in value) return serChoice(value);
      if ("use_value" in value) return serUse(value);
      return serCond(value);
    }
  ));
  // Lazy for the same reason as `jsonCodec` above: the variant helpers
  // (`NegValue.areEqual`, `Cond.areEqual`, ...) close over `Value.areEqual`
  // to recurse into nested sub-expressions.
  export const areEqual = areEqualThunk<Value>(() => (a, b) => {
    if (typeof a === "bigint") return typeof b === "bigint" && a === b;
    if (a === "time_interval_start") return b === "time_interval_start";
    if (a === "time_interval_end") return b === "time_interval_end";
    if ("amount_of_token" in a) return "amount_of_token" in b && AvailableMoney.areEqual(a, b);
    if ("negate" in a) return "negate" in b && NegValue.areEqual(a, b);
    if ("add" in a) return "add" in b && AddValue.areEqual(a, b);
    if ("value" in a && "minus" in a) return "value" in b && "minus" in b && SubValue.areEqual(a, b);
    if ("multiply" in a) return "multiply" in b && MulValue.areEqual(a, b);
    if ("divide" in a) return "divide" in b && DivValue.areEqual(a, b);
    if ("value_of_choice" in a) return "value_of_choice" in b && ChoiceValue.areEqual(a, b);
    if ("use_value" in a) return "use_value" in b && UseValue.areEqual(a, b);
    return "if" in b && Cond.areEqual(a, b);
  });
}

// Core `Observation` type -----------------------------------------------------------

export type Observation =
  | AndObs
  | OrObs
  | NotObs
  | ChoseSomething
  | ValueEQ
  | ValueGT
  | ValueGE
  | ValueLT
  | ValueLE
  | boolean;

export namespace Observation {
  // We construct this codec lazily because its variants codecs reference it forming a cycle.
  // Another reference loop goes through `Value` though its codec is lazy as well.
  export const jsonCodec: JsonCodec<Observation> = fromCodecThunkFn(() => altJsonCodecs(
    [
      AndObs.jsonCodec,
      OrObs.jsonCodec,
      NotObs.jsonCodec,
      ChoseSomething.jsonCodec,
      ValueEQ.jsonCodec,
      ValueGT.jsonCodec,
      ValueGE.jsonCodec,
      ValueLT.jsonCodec,
      ValueLE.jsonCodec,
      json2BooleanCodec,
    ],
    (
      serAnd,
      serOr,
      serNot,
      serChose,
      serEq,
      serGt,
      serGe,
      serLt,
      serLe,
      serBool
    ) => (value: Observation): Json => {
      if (typeof value === "boolean") return value;
      if ("both" in value) return serAnd(value);
      if ("either" in value) return serOr(value);
      if ("not" in value) return serNot(value);
      if ("chose_something_for" in value) return serChose(value);
      if ("equal_to" in value) return serEq(value);
      if ("gt" in value) return serGt(value);
      if ("ge_than" in value) return serGe(value);
      if ("lt" in value) return serLt(value);
      if ("le_than" in value) return serLe(value);
      return serBool(value);
    }
  ));
  // Lazy for the same reason as `jsonCodec` above: the variant helpers
  // close over `Observation.areEqual` to recurse into nested sub-terms.
  export const areEqual = areEqualThunk<Observation>(() => (a, b) => {
    if (typeof a === "boolean") return typeof b === "boolean" && a === b;
    if ("both" in a) return "both" in b && AndObs.areEqual(a, b);
    if ("either" in a) return "either" in b && OrObs.areEqual(a, b);
    if ("not" in a) return "not" in b && NotObs.areEqual(a, b);
    if ("chose_something_for" in a) return "chose_something_for" in b && ChoseSomething.areEqual(a, b);
    if ("equal_to" in a) return "equal_to" in b && ValueEQ.areEqual(a, b);
    if ("gt" in a) return "gt" in b && ValueGT.areEqual(a, b);
    if ("ge_than" in a) return "ge_than" in b && ValueGE.areEqual(a, b);
    if ("lt" in a) return "lt" in b && ValueLT.areEqual(a, b);
    if ("le_than" in a) return "le_than" in b && ValueLE.areEqual(a, b);
    // Unreachable: every object variant was exhausted above, so the only
    // way to fall through is if `a` is the `boolean` case, which was
    // already handled.
    return false;
  });
}

// Value variants ----------------------------------------------------------------

export type AvailableMoney = {
  amount_of_token: Token;
  in_account: AccountId;
}

export function AvailableMoney(amount_of_token: Token, in_account: AccountId): AvailableMoney {
  return { amount_of_token, in_account };
}
export namespace AvailableMoney {
  export const jsonCodec: JsonCodec<AvailableMoney> = objectOf({
    amount_of_token: Token.jsonCodec,
    in_account: AccountId.jsonCodec,
  });
  export const areEqual = (a: AvailableMoney, b: AvailableMoney): boolean =>
    Token.areEqual(a.amount_of_token, b.amount_of_token) &&
    AccountId.areEqual(a.in_account, b.in_account);
}

export type Constant = bigint;

export function Constant(value: bigint): Constant {
  return value;
}
export namespace Constant {
  export const jsonCodec: JsonCodec<Constant> = json2BigIntCodec;
  // `Constant` is currently a `bigint` alias. The helper is wired up so
  // that moving to a tagged/branded representation later only requires
  // swapping the body here.
  export const areEqual = (a: Constant, b: Constant): boolean => a === b;
}

export type TimeIntervalStart = "time_interval_start";

export function TimeIntervalStart(): TimeIntervalStart {
  return "time_interval_start";
}
export namespace TimeIntervalStart {
  export const jsonCodec: JsonCodec<TimeIntervalStart> = jsonConstant("time_interval_start");
  export const areEqual = (a: TimeIntervalStart, b: TimeIntervalStart): boolean => a === b;
}

export type TimeIntervalEnd = "time_interval_end";

export function TimeIntervalEnd(): TimeIntervalEnd {
  return "time_interval_end";
}
export namespace TimeIntervalEnd {
  export const jsonCodec: JsonCodec<TimeIntervalEnd> = jsonConstant("time_interval_end");
  export const areEqual = (a: TimeIntervalEnd, b: TimeIntervalEnd): boolean => a === b;
}

export type NegValue = { negate: Value };

export function NegValue(negate: Value): NegValue {
  return { negate };
}
export namespace NegValue {
  export const jsonCodec: JsonCodec<NegValue> = objectOf({
    negate: Value.jsonCodec,
  });
  export const areEqual = (a: NegValue, b: NegValue): boolean => Value.areEqual(a.negate, b.negate);
}

export type AddValue = {
  add: Value;
  and: Value;
}

export function AddValue(add: Value, and: Value): AddValue {
  return { add, and };
}
export namespace AddValue {
  export const jsonCodec: JsonCodec<AddValue> = objectOf({
    add: Value.jsonCodec,
    and: Value.jsonCodec,
  });
  export const areEqual = (a: AddValue, b: AddValue): boolean =>
    Value.areEqual(a.add, b.add) && Value.areEqual(a.and, b.and);
}

export type SubValue = {
  value: Value;
  minus: Value;
}

export function SubValue(value: Value, minus: Value): SubValue {
  return { value, minus };
}
export namespace SubValue {
  export const jsonCodec: JsonCodec<SubValue> = objectOf({
    value: Value.jsonCodec,
    minus: Value.jsonCodec,
  });
  export const areEqual = (a: SubValue, b: SubValue): boolean =>
    Value.areEqual(a.value, b.value) && Value.areEqual(a.minus, b.minus);
}

export type MulValue = {
  multiply: Value;
  times: Value;
}

export function MulValue(multiply: Value, times: Value): MulValue {
  return { multiply, times };
}

export namespace MulValue {
  export const jsonCodec: JsonCodec<MulValue> = objectOf({
    multiply: Value.jsonCodec,
    times: Value.jsonCodec,
  });
  export const areEqual = (a: MulValue, b: MulValue): boolean =>
    Value.areEqual(a.multiply, b.multiply) && Value.areEqual(a.times, b.times);
}

export type DivValue = {
  divide: Value;
  by: Value;
}

export function DivValue(divide: Value, by: Value): DivValue {
  return { divide, by };
}
export namespace DivValue {
  export const jsonCodec: JsonCodec<DivValue> = objectOf({
    divide: Value.jsonCodec,
    by: Value.jsonCodec,
  });
  export const areEqual = (a: DivValue, b: DivValue): boolean =>
    Value.areEqual(a.divide, b.divide) && Value.areEqual(a.by, b.by);
}

export type ChoiceValue = { value_of_choice: ChoiceId };

export function ChoiceValue(value_of_choice: ChoiceId): ChoiceValue {
  return { value_of_choice };
}
export namespace ChoiceValue {
  export const jsonCodec: JsonCodec<ChoiceValue> = objectOf({
    value_of_choice: ChoiceId.jsonCodec,
  });
  export const areEqual = (a: ChoiceValue, b: ChoiceValue): boolean =>
    ChoiceId.areEqual(a.value_of_choice, b.value_of_choice);
}

export type ValueId = string;

export function ValueId(value: string): ValueId {
  return value;
}
export namespace ValueId {
  export const jsonCodec: JsonCodec<ValueId> = json2StringCodec;
  // `ValueId` is currently a `string` alias. The helper is wired up so
  // that moving to a tagged/branded representation later only requires
  // swapping the body here.
  export const areEqual = (a: ValueId, b: ValueId): boolean => a === b;
}

export type UseValue = {
  use_value: ValueId;
}

export function UseValue(use_value: ValueId): UseValue {
  return { use_value };
}
export namespace UseValue {
  export const jsonCodec: JsonCodec<UseValue> = objectOf({
    use_value: ValueId.jsonCodec,
  });
  export const areEqual = (a: UseValue, b: UseValue): boolean => ValueId.areEqual(a.use_value, b.use_value);
}

export type Cond = {
  if: Observation;
  then: Value;
  else: Value;
}

export function Cond(ifObs: Observation, thenVal: Value, elseVal: Value): Cond {
  return { if: ifObs, then: thenVal, else: elseVal };
}
export namespace Cond {
  export const jsonCodec: JsonCodec<Cond> = objectOf({
    if: Observation.jsonCodec,
    then: Value.jsonCodec,
    else: Value.jsonCodec,
  });
  export const areEqual = (a: Cond, b: Cond): boolean =>
    Observation.areEqual(a.if, b.if) &&
    Value.areEqual(a.then, b.then) &&
    Value.areEqual(a.else, b.else);
}

// Observation variants ----------------------------------------------------------------

export type AndObs = {
  both: Observation;
  and: Observation;
}

export function AndObs(both: Observation, and: Observation): AndObs {
  return { both, and };
}
export namespace AndObs {
  export const jsonCodec: JsonCodec<AndObs> = objectOf({
    both: Observation.jsonCodec,
    and: Observation.jsonCodec,
  });
  export const areEqual = (a: AndObs, b: AndObs): boolean =>
    Observation.areEqual(a.both, b.both) && Observation.areEqual(a.and, b.and);
}

export type OrObs = {
  either: Observation;
  or: Observation;
}

export function OrObs(either: Observation, or: Observation): OrObs {
  return { either, or };
}
export namespace OrObs {
  export const jsonCodec: JsonCodec<OrObs> = objectOf({
    either: Observation.jsonCodec,
    or: Observation.jsonCodec,
  });
  export const areEqual = (a: OrObs, b: OrObs): boolean =>
    Observation.areEqual(a.either, b.either) && Observation.areEqual(a.or, b.or);
}

export type NotObs = {
  not: Observation;
}

export function NotObs(not: Observation): NotObs {
  return { not };
}
export namespace NotObs {
  export const jsonCodec = objectOf({
    not: Observation.jsonCodec,
  });
  export const areEqual = (a: NotObs, b: NotObs): boolean => Observation.areEqual(a.not, b.not);
}

export type ChoseSomething = {
  chose_something_for: ChoiceId;
}

export function ChoseSomething(chose_something_for: ChoiceId): ChoseSomething {
  return { chose_something_for };
}
export namespace ChoseSomething {
  export const jsonCodec: JsonCodec<ChoseSomething> = objectOf({
    chose_something_for: ChoiceId.jsonCodec,
  });
  export const areEqual = (a: ChoseSomething, b: ChoseSomething): boolean =>
    ChoiceId.areEqual(a.chose_something_for, b.chose_something_for);
}

export type ValueEQ = {
  value: Value;
  equal_to: Value;
}

export function ValueEQ(value: Value, equal_to: Value): ValueEQ {
  return { value, equal_to };
}
export namespace ValueEQ {
  export const jsonCodec: JsonCodec<ValueEQ> = objectOf({
    value: Value.jsonCodec,
    equal_to: Value.jsonCodec,
  });
  export const areEqual = (a: ValueEQ, b: ValueEQ): boolean =>
    Value.areEqual(a.value, b.value) && Value.areEqual(a.equal_to, b.equal_to);
}

export type ValueGT = {
  value: Value;
  gt: Value;
}

export function ValueGT(value: Value, gt: Value): ValueGT {
  return { value, gt };
}
export namespace ValueGT {
  export const jsonCodec: JsonCodec<ValueGT> = objectOf({
    value: Value.jsonCodec,
    gt: Value.jsonCodec,
  });
  export const areEqual = (a: ValueGT, b: ValueGT): boolean =>
    Value.areEqual(a.value, b.value) && Value.areEqual(a.gt, b.gt);
}
export type ValueGE = {
  value: Value;
  ge_than: Value;
}

export function ValueGE(value: Value, ge_than: Value): ValueGE {
  return { value, ge_than };
}
export namespace ValueGE {
  export const jsonCodec: JsonCodec<ValueGE> = objectOf({
    value: Value.jsonCodec,
    ge_than: Value.jsonCodec,
  });
  export const areEqual = (a: ValueGE, b: ValueGE): boolean =>
    Value.areEqual(a.value, b.value) && Value.areEqual(a.ge_than, b.ge_than);
}

export type ValueLT = {
  value: Value;
  lt: Value;
}

export function ValueLT(value: Value, lt: Value): ValueLT {
  return { value, lt };
}
export namespace ValueLT {
  export const jsonCodec: JsonCodec<ValueLT> = objectOf({
    value: Value.jsonCodec,
    lt: Value.jsonCodec,
  });
  export const areEqual = (a: ValueLT, b: ValueLT): boolean =>
    Value.areEqual(a.value, b.value) && Value.areEqual(a.lt, b.lt);
}

export type ValueLE = {
  value: Value;
  le_than: Value;
}

export function ValueLE(value: Value, le_than: Value): ValueLE {
  return { value, le_than };
}
export namespace ValueLE {
  export const jsonCodec: JsonCodec<ValueLE> = objectOf({
    value: Value.jsonCodec,
    le_than: Value.jsonCodec,
  });
  export const areEqual = (a: ValueLE, b: ValueLE): boolean =>
    Value.areEqual(a.value, b.value) && Value.areEqual(a.le_than, b.le_than);
}
