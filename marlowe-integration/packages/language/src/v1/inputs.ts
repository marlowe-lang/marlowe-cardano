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
import { Contract } from "./contract.js";
import { ChoiceId, ChosenNum } from "./choices.js";
import { Party } from "./participants.js";
import { AccountId } from "./payee.js";
import { Token } from "./token.js";

export type IChoice = {
  for_choice_id: ChoiceId;
  input_that_chooses_num: ChosenNum;
};

export namespace IChoice {
  export const jsonCodec: JsonCodec<IChoice> = objectOf({
    for_choice_id: ChoiceId.jsonCodec,
    input_that_chooses_num: json2BigIntCodec,
  });
  export const areEqual = (a: IChoice, b: IChoice): boolean =>
    ChoiceId.areEqual(a.for_choice_id, b.for_choice_id) &&
    ChosenNum.areEqual(a.input_that_chooses_num, b.input_that_chooses_num);
}

export type IDeposit = {
  input_from_party: Party;
  that_deposits: bigint;
  of_token: Token;
  into_account: AccountId;
};

export namespace IDeposit {
  export const jsonCodec: JsonCodec<IDeposit> = objectOf({
    input_from_party: Party.jsonCodec,
    that_deposits: json2BigIntCodec,
    of_token: Token.jsonCodec,
    into_account: AccountId.jsonCodec,
  });
  export const areEqual = (a: IDeposit, b: IDeposit): boolean =>
    Party.areEqual(a.input_from_party, b.input_from_party) &&
    a.that_deposits === b.that_deposits &&
    Token.areEqual(a.of_token, b.of_token) &&
    AccountId.areEqual(a.into_account, b.into_account);
}

export const inputNotify = "input_notify";

export type INotify = "input_notify";

export namespace INotify {
  export const jsonCodec: JsonCodec<INotify> = jsonConstant("input_notify");
  export const areEqual = (a: INotify, b: INotify): boolean => a === b;
}

export type BuiltinByteString = string;

export namespace BuiltinByteString {
  export const jsonCodec: JsonCodec<BuiltinByteString> = json2StringCodec;
  // `BuiltinByteString` is currently a `string` alias. The helper is wired
  // up so that moving to a tagged/branded representation later only
  // requires swapping the body here.
  export const areEqual = (a: BuiltinByteString, b: BuiltinByteString): boolean => a === b;
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
  export const areEqual = (a: InputContent, b: InputContent): boolean => {
    if (typeof a === "string") return typeof b === "string" && a === b;
    if ("for_choice_id" in a) return "for_choice_id" in b && IChoice.areEqual(a, b);
    return "input_from_party" in b && IDeposit.areEqual(a, b);
  };
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
    merkleized_continuation: Contract.jsonCodec,
  });
  export const areEqual = (a: MerkleizedHashAndContinuation, b: MerkleizedHashAndContinuation): boolean =>
    BuiltinByteString.areEqual(a.continuation_hash, b.continuation_hash) &&
    Contract.areEqual(a.merkleized_continuation, b.merkleized_continuation);
}

export namespace MerkleizedDeposit {
  export const jsonCodec: JsonCodec<MerkleizedDeposit> = objectOf({
    input_from_party: Party.jsonCodec,
    that_deposits: json2BigIntCodec,
    of_token: Token.jsonCodec,
    into_account: AccountId.jsonCodec,
    continuation_hash: BuiltinByteString.jsonCodec,
    merkleized_continuation: Contract.jsonCodec,
  });
  export const areEqual = (a: MerkleizedDeposit, b: MerkleizedDeposit): boolean =>
    IDeposit.areEqual(a, b) && MerkleizedHashAndContinuation.areEqual(a, b);
}

export namespace MerkleizedChoice {
  export const jsonCodec: JsonCodec<MerkleizedChoice> = objectOf({
    for_choice_id: ChoiceId.jsonCodec,
    input_that_chooses_num: json2BigIntCodec,
    continuation_hash: BuiltinByteString.jsonCodec,
    merkleized_continuation: Contract.jsonCodec,
  });
  export const areEqual = (a: MerkleizedChoice, b: MerkleizedChoice): boolean =>
    IChoice.areEqual(a, b) && MerkleizedHashAndContinuation.areEqual(a, b);
}

export namespace MerkleizedNotify {
  export const jsonCodec: JsonCodec<MerkleizedNotify> = objectOf({
    continuation_hash: BuiltinByteString.jsonCodec,
    merkleized_continuation: Contract.jsonCodec,
  });
  export const areEqual = (a: MerkleizedNotify, b: MerkleizedNotify): boolean =>
    MerkleizedHashAndContinuation.areEqual(a, b);
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
  export const areEqual = (a: MerkleizedInput, b: MerkleizedInput): boolean => {
    if ("input_from_party" in a) return "input_from_party" in b && MerkleizedDeposit.areEqual(a, b);
    if ("for_choice_id" in a) return "for_choice_id" in b && MerkleizedChoice.areEqual(a, b);
    return MerkleizedNotify.areEqual(a, b);
  };
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
  export const areEqual = (a: Input, b: Input): boolean => {
    if (typeof a === "string") return typeof b === "string" && a === b;
    if ("continuation_hash" in a) {
      return "continuation_hash" in b && MerkleizedInput.areEqual(a, b);
    }
    return "continuation_hash" in b ? false : InputContent.areEqual(a, b);
  };
}