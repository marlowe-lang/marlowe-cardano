import {
  altJsonCodecs,
  arrayOf,
  constant as jsonConstant,
  json2BigIntCodec,
  json2StringCodec,
  objectOf,
  tupleOf,
  type JsonCodec,
} from "@konduit/codec/json/codecs";
import { fromCodecThunkFn } from "@konduit/codec";
import type { Contract } from "./contract.js";
import { Contract as ContractNamespace } from "./contract.js";
import type { ChoiceId, ChosenNum } from "./choices.js";
import { ChoiceId as ChoiceIdNamespace } from "./choices.js";
import type { Party } from "./participants.js";
import { Party as PartyNamespace } from "./participants.js";
import type { AccountId } from "./payee.js";
import { AccountId as AccountIdNamespace } from "./payee.js";
import type { Token } from "./token.js";
import { Token as TokenNamespace } from "./token.js";

export type IChoice = {
  for_choice_id: ChoiceId;
  input_that_chooses_num: ChosenNum;
};

export namespace IChoice {
  export const jsonCodec: JsonCodec<IChoice> = objectOf({
    for_choice_id: ChoiceIdNamespace.jsonCodec,
    input_that_chooses_num: json2BigIntCodec,
  });
}

export type IDeposit = {
  input_from_party: Party;
  that_deposits: bigint;
  of_token: Token;
  into_account: AccountId;
};

export namespace IDeposit {
  export const jsonCodec: JsonCodec<IDeposit> = objectOf({
    input_from_party: PartyNamespace.jsonCodec,
    that_deposits: json2BigIntCodec,
    of_token: TokenNamespace.jsonCodec,
    into_account: AccountIdNamespace.jsonCodec,
  });
}

export const inputNotify = "input_notify";

export type INotify = "input_notify";

export namespace INotify {
  export const jsonCodec: JsonCodec<INotify> = jsonConstant("input_notify");
}

export type BuiltinByteString = string;

export namespace BuiltinByteString {
  export const jsonCodec: JsonCodec<BuiltinByteString> = json2StringCodec;
}

export type InputContent = IDeposit | IChoice | INotify;

export namespace InputContent {
  export const jsonCodec: JsonCodec<InputContent> = altJsonCodecs(
    [IDeposit.jsonCodec, IChoice.jsonCodec, INotify.jsonCodec],
    (serDeposit, serChoice, serNotify) => (input: InputContent) => {
      if (typeof input === "string") return serNotify(input);
      if ("for_choice_id" in input) return serChoice(input);
      return serDeposit(input);
    }
  );
}

export type NormalInput = InputContent;

export type MerkleizedHashAndContinuation = {
  continuation_hash: BuiltinByteString;
  merkleized_continuation: Contract;
};

export type MerkleizedDeposit = IDeposit & MerkleizedHashAndContinuation;

export type MerkleizedChoice = IChoice & MerkleizedHashAndContinuation;

export type MerkleizedNotify = MerkleizedHashAndContinuation;

export type MerkleizedInput = MerkleizedDeposit | MerkleizedChoice | MerkleizedNotify;

// `MerkleizedInput` references `Contract` via `MerkleizedHashAndContinuation`,
// so its codec is constructed lazily via `fromCodecThunkFn`.
export namespace MerkleizedHashAndContinuation {
  export const jsonCodec: JsonCodec<MerkleizedHashAndContinuation> = objectOf({
    continuation_hash: BuiltinByteString.jsonCodec,
    merkleized_continuation: ContractNamespace.jsonCodec,
  });
}

export namespace MerkleizedDeposit {
  export const jsonCodec: JsonCodec<MerkleizedDeposit> = objectOf({
    input_from_party: PartyNamespace.jsonCodec,
    that_deposits: json2BigIntCodec,
    of_token: TokenNamespace.jsonCodec,
    into_account: AccountIdNamespace.jsonCodec,
    continuation_hash: BuiltinByteString.jsonCodec,
    merkleized_continuation: ContractNamespace.jsonCodec,
  });
}

export namespace MerkleizedChoice {
  export const jsonCodec: JsonCodec<MerkleizedChoice> = objectOf({
    for_choice_id: ChoiceIdNamespace.jsonCodec,
    input_that_chooses_num: json2BigIntCodec,
    continuation_hash: BuiltinByteString.jsonCodec,
    merkleized_continuation: ContractNamespace.jsonCodec,
  });
}

export namespace MerkleizedNotify {
  export const jsonCodec: JsonCodec<MerkleizedNotify> = objectOf({
    continuation_hash: BuiltinByteString.jsonCodec,
    merkleized_continuation: ContractNamespace.jsonCodec,
  });
}

export namespace MerkleizedInput {
  export const jsonCodec: JsonCodec<MerkleizedInput> = fromCodecThunkFn(() =>
    altJsonCodecs(
      [MerkleizedDeposit.jsonCodec, MerkleizedChoice.jsonCodec, MerkleizedNotify.jsonCodec],
      (serDeposit, serChoice, serNotify) => (input: MerkleizedInput) => {
        if ("input_from_party" in input) return serDeposit(input);
        if ("for_choice_id" in input) return serChoice(input);
        return serNotify(input);
      }
    )
  );
}

export type Input = NormalInput | MerkleizedInput;

export namespace Input {
  export const jsonCodec: JsonCodec<Input> = fromCodecThunkFn(() =>
    altJsonCodecs(
      [InputContent.jsonCodec, MerkleizedInput.jsonCodec],
      (serNormal, serMerkleized) => (input: Input) => {
        if (typeof input === "string") {
          return serNormal(input);
        }
        if ("continuation_hash" in input) {
          return serMerkleized(input);
        }
        return serNormal(input);
      }
    )
  );
}