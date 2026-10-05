import { json2StringCodec } from "@konduit/codec/json/codecs";
import type { JsonCodec } from "@konduit/codec/json/codecs";

export type AddressBech32 = string;
export function AddressBech32(value: string): AddressBech32 {
  return value;
}
export namespace AddressBech32 {
  export const jsonCodec: JsonCodec<AddressBech32> = json2StringCodec;
  export const areEqual = (a: AddressBech32, b: AddressBech32): boolean => a === b;
}
