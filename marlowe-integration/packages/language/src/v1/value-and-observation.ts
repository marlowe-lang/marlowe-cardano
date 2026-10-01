import {
  altJsonCodecs,
  constant,
  json2BigIntCodec,
  json2BooleanCodec,
  json2StringCodec,
  objectOf,
  type JsonCodec,
} from "@konduit/codec/json/codecs";
import type { ChoiceId } from "./choices.js";
import { ChoiceId as ChoiceIdNamespace } from "./choices.js";
import type { AccountId } from "./payee.js";
import { AccountId as AccountIdNamespace } from "./payee.js";
import type { Token } from "./token.js";
import { Token as TokenNamespace } from "./token.js";

// Observation is needed by Cond, and Value is needed by the comparison
// observations. We therefore forward-declare both codecs lazily using
// `let` bindings and wire them up at the end of the module.

export interface AvailableMoney {
  amount_of_token: Token;
  in_account: AccountId;
}

export const constant = (constant: bigint) => constant;

export type Constant = bigint;

export type TimeIntervalStart = "time_interval_start";

export type TimeIntervalEnd = "time_interval_end";

export interface NegValue {
  negate: Value;
}

export interface AddValue {
  add: Value;
  and: Value;
}

export interface SubValue {
  value: Value;
  minus: Value;
}

export const mulValue = (multiply: Value, times: Value) => ({
  multiply: multiply,
  times: times,
});

export interface MulValue {
  multiply: Value;
  times: Value;
}

export interface DivValue {
  divide: Value;
  by: Value;
}

export type ChoiceValue = { value_of_choice: ChoiceId };

export type ValueId = string;

export interface UseValue {
  use_value: ValueId;
}

export interface Cond {
  if: Observation;
  then: Value;
  else: Value;
}

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

export interface AndObs {
  both: Observation;
  and: Observation;
}

export interface OrObs {
  either: Observation;
  or: Observation;
}

export interface NotObs {
  not: Observation;
}

export interface ChoseSomething {
  chose_something_for: ChoiceId;
}

export interface ValueEQ {
  value: Value;
  equal_to: Value;
}

export interface ValueGT {
  value: Value;
  gt: Value;
}

export interface ValueGE {
  value: Value;
  ge_than: Value;
}

export interface ValueLT {
  value: Value;
  lt: Value;
}

export interface ValueLE {
  value: Value;
  le_than: Value;
}

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

// Leaf codecs (do not depend on Value/Observation)

export namespace AvailableMoney {
  export const jsonCodec: JsonCodec<AvailableMoney> = objectOf({
    amount_of_token: TokenNamespace.jsonCodec,
    in_account: AccountIdNamespace.jsonCodec,
  });
}

export namespace Constant {
  export const jsonCodec: JsonCodec<Constant> = json2BigIntCodec;
}

export namespace TimeIntervalStart {
  export const jsonCodec: JsonCodec<TimeIntervalStart> = constant("time_interval_start");
}

export namespace TimeIntervalEnd {
  export const jsonCodec: JsonCodec<TimeIntervalEnd> = constant("time_interval_end");
}

export namespace ChoiceValue {
  export const jsonCodec: JsonCodec<ChoiceValue> = objectOf({
    value_of_choice: ChoiceIdNamespace.jsonCodec,
  });
}

export namespace ValueId {
  export const jsonCodec: JsonCodec<ValueId> = json2StringCodec;
}

export namespace UseValue {
  export const jsonCodec: JsonCodec<UseValue> = objectOf({
    use_value: ValueId.jsonCodec,
  });
}

export namespace ChoseSomething {
  export const jsonCodec: JsonCodec<ChoseSomething> = objectOf({
    chose_something_for: ChoiceIdNamespace.jsonCodec,
  });
}

export namespace AndObs {
  export const jsonCodec: JsonCodec<AndObs> = objectOf({
    both: Observation.jsonCodec,
    and: Observation.jsonCodec,
  });
}

export namespace OrObs {
  export const jsonCodec: JsonCodec<OrObs> = objectOf({
    either: Observation.jsonCodec,
    or: Observation.jsonCodec,
  });
}

export namespace NotObs {
  export const jsonCodec: JsonCodec<NotObs> = objectOf({
    not: Observation.jsonCodec,
  });
}

// Mutable holders for the recursive codecs. We resolve the cycle by
// assigning these in place once every leaf codec has been declared.
export let _ValueJsonCodec!: JsonCodec<Value>;
export let _ObservationJsonCodec!: JsonCodec<Observation>;

export namespace Value {
  export const jsonCodec: JsonCodec<Value> = _ValueJsonCodec;
}

export namespace Observation {
  export const jsonCodec: JsonCodec<Observation> = _ObservationJsonCodec;
}

// Recursive codecs - depend on Value/Observation namespaces

export namespace NegValue {
  export const jsonCodec: JsonCodec<NegValue> = objectOf({
    negate: Value.jsonCodec,
  });
}

export namespace AddValue {
  export const jsonCodec: JsonCodec<AddValue> = objectOf({
    add: Value.jsonCodec,
    and: Value.jsonCodec,
  });
}

export namespace SubValue {
  export const jsonCodec: JsonCodec<SubValue> = objectOf({
    value: Value.jsonCodec,
    minus: Value.jsonCodec,
  });
}

export namespace MulValue {
  export const jsonCodec: JsonCodec<MulValue> = objectOf({
    multiply: Value.jsonCodec,
    times: Value.jsonCodec,
  });
}

export namespace DivValue {
  export const jsonCodec: JsonCodec<DivValue> = objectOf({
    divide: Value.jsonCodec,
    by: Value.jsonCodec,
  });
}

export namespace Cond {
  export const jsonCodec: JsonCodec<Cond> = objectOf({
    if: Observation.jsonCodec,
    then: Value.jsonCodec,
    else: Value.jsonCodec,
  });
}

export namespace ValueEQ {
  export const jsonCodec: JsonCodec<ValueEQ> = objectOf({
    value: Value.jsonCodec,
    equal_to: Value.jsonCodec,
  });
}

export namespace ValueGT {
  export const jsonCodec: JsonCodec<ValueGT> = objectOf({
    value: Value.jsonCodec,
    gt: Value.jsonCodec,
  });
}

export namespace ValueGE {
  export const jsonCodec: JsonCodec<ValueGE> = objectOf({
    value: Value.jsonCodec,
    ge_than: Value.jsonCodec,
  });
}

export namespace ValueLT {
  export const jsonCodec: JsonCodec<ValueLT> = objectOf({
    value: Value.jsonCodec,
    lt: Value.jsonCodec,
  });
}

export namespace ValueLE {
  export const jsonCodec: JsonCodec<ValueLE> = objectOf({
    value: Value.jsonCodec,
    le_than: Value.jsonCodec,
  });
}

// Wire up the recursive codecs now that every leaf codec has been declared.

_ValueJsonCodec = altJsonCodecs(
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
    serAvailableMoney,
    serConstant,
    serNeg,
    serAdd,
    serSub,
    serMul,
    serDiv,
    serChoiceValue,
    serTimeStart,
    serTimeEnd,
    serUseValue,
    serCond
  ) => (value: Value) => {
    if (typeof value === "bigint") {
      return serConstant(value);
    }
    if (value === "time_interval_start") {
      return serTimeStart(value);
    }
    if (value === "time_interval_end") {
      return serTimeEnd(value);
    }
    if ("amount_of_token" in value) {
      return serAvailableMoney(value);
    }
    if ("negate" in value) {
      return serNeg(value);
    }
    if ("add" in value) {
      return serAdd(value);
    }
    if ("value" in value && "minus" in value) {
      return serSub(value);
    }
    if ("multiply" in value) {
      return serMul(value);
    }
    if ("divide" in value) {
      return serDiv(value);
    }
    if ("value_of_choice" in value) {
      return serChoiceValue(value);
    }
    if ("use_value" in value) {
      return serUseValue(value);
    }
    // cond
    return serCond(value);
  }
);

_ObservationJsonCodec = altJsonCodecs(
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
    serChoseSomething,
    serEQ,
    serGT,
    serGE,
    serLT,
    serLE,
    serBool
  ) => (obs: Observation) => {
    if (typeof obs === "boolean") {
      return serBool(obs);
    }
    if ("both" in obs) {
      return serAnd(obs);
    }
    if ("either" in obs) {
      return serOr(obs);
    }
    if ("not" in obs) {
      return serNot(obs);
    }
    if ("chose_something_for" in obs) {
      return serChoseSomething(obs);
    }
    if ("equal_to" in obs) {
      return serEQ(obs);
    }
    if ("gt" in obs) {
      return serGT(obs);
    }
    if ("ge_than" in obs) {
      return serGE(obs);
    }
    if ("lt" in obs) {
      return serLT(obs);
    }
    // le_than
    return serLE(obs);
  }
);
