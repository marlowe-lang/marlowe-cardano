import type { Sort } from "../assoc-map.js";
import {
  json2BigIntCodec,
  json2StringCodec,
  objectOf,
  type JsonCodec,
} from "@konduit/codec/json/codecs";
import { Party } from "./participants.js";

// Types ---------------------------------------------------------------------

export type ChoiceName = string;

export namespace ChoiceName {
  export const jsonCodec: JsonCodec<ChoiceName> = json2StringCodec;
  export const areEqual = (a: ChoiceName, b: ChoiceName): boolean => a === b;
}

export type ChoiceId = {
  choice_name: ChoiceName;
  choice_owner: Party;
};

export function ChoiceId(choiceName: ChoiceName, choiceOwner: Party): ChoiceId {
  return { choice_name: choiceName, choice_owner: choiceOwner };
}

export namespace ChoiceId {
  export const jsonCodec: JsonCodec<ChoiceId> = objectOf({
    choice_name: ChoiceName.jsonCodec,
    choice_owner: Party.jsonCodec,
  });
  export const areEqual = (a: ChoiceId, b: ChoiceId): boolean =>
    ChoiceName.areEqual(a.choice_name, b.choice_name) &&
    Party.areEqual(a.choice_owner, b.choice_owner);
}

export type Bound = {
  from: bigint;
  to: bigint;
};

export type ChosenNum = bigint;

export function Bound(from: bigint, to: bigint): Bound {
  return { from, to };
}
export namespace Bound {
  export const jsonCodec: JsonCodec<Bound> = objectOf({
    from: json2BigIntCodec,
    to: json2BigIntCodec,
  });
  export const areEqual = (a: Bound, b: Bound): boolean => a.from === b.from && a.to === b.to;
}

export namespace ChosenNum {
  export const jsonCodec: JsonCodec<ChosenNum> = json2BigIntCodec;
  // `ChosenNum` is currently a `bigint` alias. The helper is wired up so
  // that moving to a tagged/branded representation later only requires
  // swapping the body here.
  export const areEqual = (a: ChosenNum, b: ChosenNum): boolean => a === b;
}

// Helpers --------------------------------------------------------------------

export function choiceIdCmp(a: ChoiceId, b: ChoiceId): Sort {
  const nameCmp = strCmp(a.choice_name, b.choice_name);
  if (nameCmp !== "EqualTo") {
    return nameCmp;
  }
  return partyCmp(a.choice_owner, b.choice_owner);
}

export function inBound(num: bigint, bound: Bound): boolean {
  return num >= bound.from && num <= bound.to;
}

export function inBounds(num: bigint, bounds: Bound[]): boolean {
  return bounds.some((bound) => inBound(num, bound));
}

function partyCmp(a: Party, b: Party): Sort {
  if ("role_token" in a && !("role_token" in b)) {
    return "LowerThan";
  }
  if (!("role_token" in a) && "role_token" in b) {
    return "GreaterThan";
  }
  if ("address" in a && "address" in b) {
    return strCmp(a.address, b.address);
  }
  if ("role_token" in a && "role_token" in b) {
    return strCmp(a.role_token, b.role_token);
  }
  throw new Error("Unreachable");
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
