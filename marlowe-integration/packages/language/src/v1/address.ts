import { json2StringCodec } from "@konduit/codec/json/codecs";
import type { JsonCodec } from "@konduit/codec/json/codecs";

export type AddressBech32 = string;

export namespace AddressBech32 {
  export const jsonCodec: JsonCodec<AddressBech32> = json2StringCodec;
}
