import * as jsonCodecs from "@konduit/codec/json/codecs";
import type { JsonCodec } from "@konduit/codec/json/codecs";

export type PolicyId = string;

export namespace PolicyId {
  export const jsonCodec: JsonCodec<PolicyId> = jsonCodecs.json2StringCodec;
}
