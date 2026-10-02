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

export type ChoiceId = {
  choice_name: ChoiceName;
  choice_owner: Party;
};

export type Bound = {
  from: bigint;
  to: bigint;
};

export type ChosenNum = bigint;

// Codecs + smart constructors ------------------------------------------------

export namespace ChoiceName {
  export const jsonCodec: JsonCodec<ChoiceName> = json2StringCodec;
}

export function ChoiceId(choiceName: ChoiceName, choiceOwner: Party): ChoiceId {
  return { choice_name: choiceName, choice_owner: choiceOwner };
}
export namespace ChoiceId {
  export const jsonCodec: JsonCodec<ChoiceId> = objectOf({
    choice_name: ChoiceName.jsonCodec,
    choice_owner: Party.jsonCodec,
  });
}

export function Bound(from: bigint, to: bigint): Bound {
  return { from, to };
}
export namespace Bound {
  export const jsonCodec: JsonCodec<Bound> = objectOf({
    from: json2BigIntCodec,
    to: json2BigIntCodec,
  });
}

export namespace ChosenNum {
  export const jsonCodec: JsonCodec<ChosenNum> = json2BigIntCodec;
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