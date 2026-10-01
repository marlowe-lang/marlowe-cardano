import type { Sort } from "../assoc-map.js";
import {
  json2BigIntCodec,
  json2StringCodec,
  objectOf,
  type JsonCodec,
} from "@konduit/codec/json/codecs";
import type { Party } from "./participants.js";
import { Party as PartyNamespace } from "./participants.js";

export type ChoiceName = string;

export namespace ChoiceName {
  export const jsonCodec: JsonCodec<ChoiceName> = json2StringCodec;
}

export interface ChoiceId {
  choice_name: ChoiceName;
  choice_owner: Party;
}

export namespace ChoiceId {
  export const jsonCodec: JsonCodec<ChoiceId> = objectOf({
    choice_name: ChoiceName.jsonCodec,
    choice_owner: PartyNamespace.jsonCodec,
  });
}

export function choiceIdCmp(a: ChoiceId, b: ChoiceId): Sort {
  const nameCmp = strCmp(a.choice_name, b.choice_name);
  if (nameCmp !== "EqualTo") {
    return nameCmp;
  }
  return partyCmp(a.choice_owner, b.choice_owner);
}

export interface Bound {
  from: bigint;
  to: bigint;
}

export namespace Bound {
  export const jsonCodec: JsonCodec<Bound> = objectOf({
    from: json2BigIntCodec,
    to: json2BigIntCodec,
  });
}

export type ChosenNum = bigint;

export namespace ChosenNum {
  export const jsonCodec: JsonCodec<ChosenNum> = json2BigIntCodec;
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
