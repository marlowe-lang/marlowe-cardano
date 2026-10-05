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
import { err, ok } from "neverthrow";
import type { Result } from "neverthrow";

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
  export const isAvailableMoney = (value: Value): value is AvailableMoney =>
    typeof value === "object" && value !== null && "amount_of_token" in value;
  export const isConstant = (value: Value): value is Constant => typeof value === "bigint";
  export const isNegValue = (value: Value): value is NegValue =>
    typeof value === "object" && value !== null && "negate" in value;
  export const isAddValue = (value: Value): value is AddValue =>
    typeof value === "object" && value !== null && "add" in value;
  export const isSubValue = (value: Value): value is SubValue =>
    typeof value === "object" && value !== null && "value" in value && "minus" in value;
  export const isMulValue = (value: Value): value is MulValue =>
    typeof value === "object" && value !== null && "multiply" in value;
  export const isDivValue = (value: Value): value is DivValue =>
    typeof value === "object" && value !== null && "divide" in value;
  export const isChoiceValue = (value: Value): value is ChoiceValue =>
    typeof value === "object" && value !== null && "value_of_choice" in value;
  export const isTimeIntervalStart = (value: Value): value is TimeIntervalStart =>
    value === "time_interval_start";
  export const isTimeIntervalEnd = (value: Value): value is TimeIntervalEnd =>
    value === "time_interval_end";
  export const isUseValue = (value: Value): value is UseValue =>
    typeof value === "object" && value !== null && "use_value" in value;
  export const isCond = (value: Value): value is Cond =>
    typeof value === "object" && value !== null && "if" in value;

  export const match = <T>(
    value: Value,
    handlers: {
      available_money: (v: AvailableMoney) => T,
      constant: (v: Constant) => T,
      neg_value: (v: NegValue) => T,
      add_value: (v: AddValue) => T,
      sub_value: (v: SubValue) => T,
      mul_value: (v: MulValue) => T,
      div_value: (v: DivValue) => T,
      choice_value: (v: ChoiceValue) => T,
      time_interval_start: (v: TimeIntervalStart) => T,
      time_interval_end: (v: TimeIntervalEnd) => T,
      use_value: (v: UseValue) => T,
      cond: (v: Cond) => T,
    },
  ): T =>
    isAvailableMoney(value) ? handlers.available_money(value)
    : isConstant(value) ? handlers.constant(value)
    : isNegValue(value) ? handlers.neg_value(value)
    : isAddValue(value) ? handlers.add_value(value)
    : isSubValue(value) ? handlers.sub_value(value)
    : isMulValue(value) ? handlers.mul_value(value)
    : isDivValue(value) ? handlers.div_value(value)
    : isChoiceValue(value) ? handlers.choice_value(value)
    : isTimeIntervalStart(value) ? handlers.time_interval_start(value)
    : isTimeIntervalEnd(value) ? handlers.time_interval_end(value)
    : isUseValue(value) ? handlers.use_value(value)
    : handlers.cond(value);

  export const tryMatch = <T>(
    value: Value,
    handlers: {
      available_money?: (v: AvailableMoney) => T,
      constant?: (v: Constant) => T,
      neg_value?: (v: NegValue) => T,
      add_value?: (v: AddValue) => T,
      sub_value?: (v: SubValue) => T,
      mul_value?: (v: MulValue) => T,
      div_value?: (v: DivValue) => T,
      choice_value?: (v: ChoiceValue) => T,
      time_interval_start?: (v: TimeIntervalStart) => T,
      time_interval_end?: (v: TimeIntervalEnd) => T,
      use_value?: (v: UseValue) => T,
      cond?: (v: Cond) => T,
    },
  ): Result<T, string> =>
    isAvailableMoney(value) ? handlers.available_money ? ok(handlers.available_money(value)) : err("Missing available_money handler")
    : isConstant(value) ? handlers.constant ? ok(handlers.constant(value)) : err("Missing constant handler")
    : isNegValue(value) ? handlers.neg_value ? ok(handlers.neg_value(value)) : err("Missing neg_value handler")
    : isAddValue(value) ? handlers.add_value ? ok(handlers.add_value(value)) : err("Missing add_value handler")
    : isSubValue(value) ? handlers.sub_value ? ok(handlers.sub_value(value)) : err("Missing sub_value handler")
    : isMulValue(value) ? handlers.mul_value ? ok(handlers.mul_value(value)) : err("Missing mul_value handler")
    : isDivValue(value) ? handlers.div_value ? ok(handlers.div_value(value)) : err("Missing div_value handler")
    : isChoiceValue(value) ? handlers.choice_value ? ok(handlers.choice_value(value)) : err("Missing choice_value handler")
    : isTimeIntervalStart(value) ? handlers.time_interval_start ? ok(handlers.time_interval_start(value)) : err("Missing time_interval_start handler")
    : isTimeIntervalEnd(value) ? handlers.time_interval_end ? ok(handlers.time_interval_end(value)) : err("Missing time_interval_end handler")
    : isUseValue(value) ? handlers.use_value ? ok(handlers.use_value(value)) : err("Missing use_value handler")
    : handlers.cond ? ok(handlers.cond(value)) : err("Missing cond handler");

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
    ) => (value: Value): Json => match(value, {
      available_money: serAvail,
      constant: serConst,
      neg_value: serNeg,
      add_value: serAdd,
      sub_value: serSub,
      mul_value: serMul,
      div_value: serDiv,
      choice_value: serChoice,
      time_interval_start: serStart,
      time_interval_end: serEnd,
      use_value: serUse,
      cond: serCond,
    })
  ));
  // Lazy for the same reason as `jsonCodec` above: the variant helpers
  // (`NegValue.areEqual`, `Cond.areEqual`, ...) close over `Value.areEqual`
  // to recurse into nested sub-expressions.
  export const areEqual = areEqualThunk<Value>(() => (a, b) =>
    isConstant(a) ? isConstant(b) && Constant.areEqual(a, b)
    : isTimeIntervalStart(a) ? isTimeIntervalStart(b) && TimeIntervalStart.areEqual(a, b)
    : isTimeIntervalEnd(a) ? isTimeIntervalEnd(b) && TimeIntervalEnd.areEqual(a, b)
    : isAvailableMoney(a) ? isAvailableMoney(b) && AvailableMoney.areEqual(a, b)
    : isNegValue(a) ? isNegValue(b) && NegValue.areEqual(a, b)
    : isAddValue(a) ? isAddValue(b) && AddValue.areEqual(a, b)
    : isSubValue(a) ? isSubValue(b) && SubValue.areEqual(a, b)
    : isMulValue(a) ? isMulValue(b) && MulValue.areEqual(a, b)
    : isDivValue(a) ? isDivValue(b) && DivValue.areEqual(a, b)
    : isChoiceValue(a) ? isChoiceValue(b) && ChoiceValue.areEqual(a, b)
    : isUseValue(a) ? isUseValue(b) && UseValue.areEqual(a, b)
    : isCond(b) && Cond.areEqual(a, b)
  );
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
  export const isAndObs = (observation: Observation): observation is AndObs =>
    typeof observation === "object" && observation !== null && "both" in observation;
  export const isOrObs = (observation: Observation): observation is OrObs =>
    typeof observation === "object" && observation !== null && "either" in observation;
  export const isNotObs = (observation: Observation): observation is NotObs =>
    typeof observation === "object" && observation !== null && "not" in observation;
  export const isChoseSomething = (observation: Observation): observation is ChoseSomething =>
    typeof observation === "object" && observation !== null && "chose_something_for" in observation;
  export const isValueEQ = (observation: Observation): observation is ValueEQ =>
    typeof observation === "object" && observation !== null && "equal_to" in observation;
  export const isValueGT = (observation: Observation): observation is ValueGT =>
    typeof observation === "object" && observation !== null && "gt" in observation;
  export const isValueGE = (observation: Observation): observation is ValueGE =>
    typeof observation === "object" && observation !== null && "ge_than" in observation;
  export const isValueLT = (observation: Observation): observation is ValueLT =>
    typeof observation === "object" && observation !== null && "lt" in observation;
  export const isValueLE = (observation: Observation): observation is ValueLE =>
    typeof observation === "object" && observation !== null && "le_than" in observation;
  export const isBoolean = (observation: Observation): observation is boolean =>
    typeof observation === "boolean";

  export const match = <T>(
    observation: Observation,
    handlers: {
      and: (v: AndObs) => T,
      or: (v: OrObs) => T,
      not: (v: NotObs) => T,
      chose_something: (v: ChoseSomething) => T,
      value_eq: (v: ValueEQ) => T,
      value_gt: (v: ValueGT) => T,
      value_ge: (v: ValueGE) => T,
      value_lt: (v: ValueLT) => T,
      value_le: (v: ValueLE) => T,
      boolean: (v: boolean) => T,
    },
  ): T =>
    isAndObs(observation) ? handlers.and(observation)
    : isOrObs(observation) ? handlers.or(observation)
    : isNotObs(observation) ? handlers.not(observation)
    : isChoseSomething(observation) ? handlers.chose_something(observation)
    : isValueEQ(observation) ? handlers.value_eq(observation)
    : isValueGT(observation) ? handlers.value_gt(observation)
    : isValueGE(observation) ? handlers.value_ge(observation)
    : isValueLT(observation) ? handlers.value_lt(observation)
    : isValueLE(observation) ? handlers.value_le(observation)
    : handlers.boolean(observation);

  export const tryMatch = <T>(
    observation: Observation,
    handlers: {
      and?: (v: AndObs) => T,
      or?: (v: OrObs) => T,
      not?: (v: NotObs) => T,
      chose_something?: (v: ChoseSomething) => T,
      value_eq?: (v: ValueEQ) => T,
      value_gt?: (v: ValueGT) => T,
      value_ge?: (v: ValueGE) => T,
      value_lt?: (v: ValueLT) => T,
      value_le?: (v: ValueLE) => T,
      boolean?: (v: boolean) => T,
    },
  ): Result<T, string> =>
    isAndObs(observation) ? handlers.and ? ok(handlers.and(observation)) : err("Missing and handler")
    : isOrObs(observation) ? handlers.or ? ok(handlers.or(observation)) : err("Missing or handler")
    : isNotObs(observation) ? handlers.not ? ok(handlers.not(observation)) : err("Missing not handler")
    : isChoseSomething(observation) ? handlers.chose_something ? ok(handlers.chose_something(observation)) : err("Missing chose_something handler")
    : isValueEQ(observation) ? handlers.value_eq ? ok(handlers.value_eq(observation)) : err("Missing value_eq handler")
    : isValueGT(observation) ? handlers.value_gt ? ok(handlers.value_gt(observation)) : err("Missing value_gt handler")
    : isValueGE(observation) ? handlers.value_ge ? ok(handlers.value_ge(observation)) : err("Missing value_ge handler")
    : isValueLT(observation) ? handlers.value_lt ? ok(handlers.value_lt(observation)) : err("Missing value_lt handler")
    : isValueLE(observation) ? handlers.value_le ? ok(handlers.value_le(observation)) : err("Missing value_le handler")
    : handlers.boolean ? ok(handlers.boolean(observation)) : err("Missing boolean handler");

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
    ) => (value: Observation): Json => match(value, {
      and: serAnd,
      or: serOr,
      not: serNot,
      chose_something: serChose,
      value_eq: serEq,
      value_gt: serGt,
      value_ge: serGe,
      value_lt: serLt,
      value_le: serLe,
      boolean: serBool,
    })
  ));
  // Lazy for the same reason as `jsonCodec` above: the variant helpers
  // close over `Observation.areEqual` to recurse into nested sub-terms.
  export const areEqual = areEqualThunk<Observation>(() => (a, b) =>
    isBoolean(a) ? isBoolean(b) && a === b
    : isAndObs(a) ? isAndObs(b) && AndObs.areEqual(a, b)
    : isOrObs(a) ? isOrObs(b) && OrObs.areEqual(a, b)
    : isNotObs(a) ? isNotObs(b) && NotObs.areEqual(a, b)
    : isChoseSomething(a) ? isChoseSomething(b) && ChoseSomething.areEqual(a, b)
    : isValueEQ(a) ? isValueEQ(b) && ValueEQ.areEqual(a, b)
    : isValueGT(a) ? isValueGT(b) && ValueGT.areEqual(a, b)
    : isValueGE(a) ? isValueGE(b) && ValueGE.areEqual(a, b)
    : isValueLT(a) ? isValueLT(b) && ValueLT.areEqual(a, b)
    : isValueLE(b) && ValueLE.areEqual(a, b)
  );
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
