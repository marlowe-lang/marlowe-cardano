import * as jsonCodecs from "@konduit/codec/json/codecs";
import type { JsonCodec } from "@konduit/codec/json/codecs";

export type PolicyId = string;

export namespace PolicyId {
  export const jsonCodec: JsonCodec<PolicyId> = jsonCodecs.json2StringCodec;
  // `PolicyId` is currently a `string` alias. The helper is wired up so that
  // moving to a tagged/branded representation later only requires swapping the
  // body here — all call sites already go through `PolicyId.areEqual`.
  export const areEqual = (a: PolicyId, b: PolicyId): boolean => a === b;
}
