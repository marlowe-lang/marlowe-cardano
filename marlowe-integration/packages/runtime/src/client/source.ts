import * as codec from "@konduit/codec";
import type { Codec } from "@konduit/codec";
import * as jsonCodecs from "@konduit/codec/json/codecs";
import type { JsonCodec, JsonError } from "@konduit/codec/json/codecs";
import { err, ok, Result } from "neverthrow";
import type { Tagged } from "type-fest";

// This should be 64 hex chars - 32 bytes of sha256 of a (sub)contract.
export type ContractSourceId = Tagged<string, "ContractSourceId">;

export namespace ContractSourceId {
  const pattern: RegExp = /^[0-9a-fA-F]{64}/;
  export const fromString = (s: string): Result<ContractSourceId, JsonError> => {
    if(pattern.test(s)) {
      return ok(s as ContractSourceId);
    } else {
      return err({msg: `Invalid ContractSourceId format: ${s}`, value: s});
    }
  }
  export const stringCodec: Codec<string, ContractSourceId, JsonError> = {
    deserialise: fromString,
    serialise: (id: ContractSourceId) => id as string,
  };

  export const jsonCodec: JsonCodec<ContractSourceId> = codec.pipe(jsonCodecs.json2StringCodec, stringCodec);
  export const urlEncode = (id: ContractSourceId): string => encodeURIComponent(id as string);
}

type PostContractSourceResponseRecord = {
  contractSourceId: ContractSourceId;
  intermediateIds: {
      [label: string]: ContractSourceId;
  };
};

export type PostContractSourceResponse = Tagged<PostContractSourceResponseRecord, "PostContractSourceResponse">

export namespace PostContractSourceResponse {
  export const jsonCodec: JsonCodec<PostContractSourceResponse> = codec.pipe(
      jsonCodecs.objectOf({
      contractSourceId: ContractSourceId.jsonCodec,
      intermediateIds: jsonCodecs.dictOf(ContractSourceId.jsonCodec),
    }), {
      deserialise: (obj) => ok(obj as PostContractSourceResponse),
      serialise: (obj) => obj as PostContractSourceResponseRecord,
    }
  );
}
