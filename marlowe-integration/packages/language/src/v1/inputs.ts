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
import { err, ok } from "neverthrow";
import type { Result } from "neverthrow";
import { Contract } from "./contract.js";
import { ChoiceId, ChosenNum } from "./choices.js";
import { Party } from "./participants.js";
import { AccountId } from "./payee.js";
import { Token } from "./token.js";

export type IChoice = {
  for_choice_id: ChoiceId;
  input_that_chooses_num: ChosenNum;
};
export function IChoice(for_choice_id: ChoiceId, input_that_chooses_num: ChosenNum): IChoice {
  return { for_choice_id, input_that_chooses_num };
}
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
export function IDeposit(input_from_party: Party, that_deposits: bigint, of_token: Token, into_account: AccountId): IDeposit {
  return { input_from_party, that_deposits, of_token, into_account };
}
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
export function INotify(): INotify {
  return "input_notify";
}
export namespace INotify {
  export const jsonCodec: JsonCodec<INotify> = jsonConstant("input_notify");
  export const areEqual = (a: INotify, b: INotify): boolean => a === b;
}

export type BuiltinByteString = string;
export function BuiltinByteString(value: string): BuiltinByteString {
  return value;
}
export namespace BuiltinByteString {
  export const jsonCodec: JsonCodec<BuiltinByteString> = json2StringCodec;
  // `BuiltinByteString` is currently a `string` alias. The helper is wired
  // up so that moving to a tagged/branded representation later only
  // requires swapping the body here.
  export const areEqual = (a: BuiltinByteString, b: BuiltinByteString): boolean => a === b;
}

export type InputContent = IDeposit | IChoice | INotify;
export namespace InputContent {
  export const isIDeposit = (input: InputContent): input is IDeposit =>
    typeof input === "object" && input !== null && "input_from_party" in input;
  export const isIChoice = (input: InputContent): input is IChoice =>
    typeof input === "object" && input !== null && "for_choice_id" in input;
  export const isINotify = (input: InputContent): input is INotify =>
    typeof input === "string";

  export const match = <T>(
    input: InputContent,
    handlers: {
      deposit: (v: IDeposit) => T,
      choice: (v: IChoice) => T,
      notify: (v: INotify) => T,
    },
  ): T =>
    isIDeposit(input) ? handlers.deposit(input)
    : isIChoice(input) ? handlers.choice(input)
    : handlers.notify(input);

  export const tryMatch = <T>(
    input: InputContent,
    handlers: {
      deposit?: (v: IDeposit) => T,
      choice?: (v: IChoice) => T,
      notify?: (v: INotify) => T,
    },
  ): Result<T, string> =>
    isIDeposit(input) ? handlers.deposit ? ok(handlers.deposit(input)) : err("Missing deposit handler")
    : isIChoice(input) ? handlers.choice ? ok(handlers.choice(input)) : err("Missing choice handler")
    : handlers.notify ? ok(handlers.notify(input)) : err("Missing notify handler");

  export const jsonCodec: JsonCodec<InputContent> = altJsonCodecs(
    [IDeposit.jsonCodec, IChoice.jsonCodec, INotify.jsonCodec],
    (serDeposit, serChoice, serNotify) => (input: InputContent) => match(input, {
      deposit: serDeposit,
      choice: serChoice,
      notify: serNotify,
    })
  );
  export const areEqual = (a: InputContent, b: InputContent): boolean =>
    isINotify(a) ? isINotify(b) && INotify.areEqual(a, b)
    : isIDeposit(a) ? isIDeposit(b) && IDeposit.areEqual(a, b)
    : isIChoice(b) && IChoice.areEqual(a, b);
}

export type NormalInput = InputContent;

export type MerkleizedHashAndContinuation = {
  continuation_hash: BuiltinByteString;
  merkleized_continuation: Contract;
};
export function MerkleizedHashAndContinuation(continuation_hash: BuiltinByteString, merkleized_continuation: Contract): MerkleizedHashAndContinuation {
  return { continuation_hash, merkleized_continuation };
}
export namespace MerkleizedHashAndContinuation {
  export const jsonCodec: JsonCodec<MerkleizedHashAndContinuation> = objectOf({
    continuation_hash: BuiltinByteString.jsonCodec,
    merkleized_continuation: Contract.jsonCodec,
  });
  export const areEqual = (a: MerkleizedHashAndContinuation, b: MerkleizedHashAndContinuation): boolean =>
    BuiltinByteString.areEqual(a.continuation_hash, b.continuation_hash) &&
    Contract.areEqual(a.merkleized_continuation, b.merkleized_continuation);
}

export type MerkleizedDeposit = IDeposit & MerkleizedHashAndContinuation;
export function MerkleizedDeposit(
  input_from_party: Party,
  that_deposits: bigint,
  of_token: Token,
  into_account: AccountId,
  continuation_hash: BuiltinByteString,
  merkleized_continuation: Contract
): MerkleizedDeposit {
  return { input_from_party, that_deposits, of_token, into_account, continuation_hash, merkleized_continuation };
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

export type MerkleizedChoice = IChoice & MerkleizedHashAndContinuation;
export function MerkleizedChoice(
  for_choice_id: ChoiceId,
  input_that_chooses_num: ChosenNum,
  continuation_hash: BuiltinByteString,
  merkleized_continuation: Contract
): MerkleizedChoice {
  return { for_choice_id, input_that_chooses_num, continuation_hash, merkleized_continuation };
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

export type MerkleizedNotify = MerkleizedHashAndContinuation;
export function MerkleizedNotify(continuation_hash: BuiltinByteString, merkleized_continuation: Contract): MerkleizedNotify {
  return { continuation_hash, merkleized_continuation };
}
export namespace MerkleizedNotify {
  export const jsonCodec: JsonCodec<MerkleizedNotify> = objectOf({
    continuation_hash: BuiltinByteString.jsonCodec,
    merkleized_continuation: Contract.jsonCodec,
  });
  export const areEqual = (a: MerkleizedNotify, b: MerkleizedNotify): boolean =>
    MerkleizedHashAndContinuation.areEqual(a, b);
}

export type MerkleizedInput = MerkleizedDeposit | MerkleizedChoice | MerkleizedNotify;
export namespace MerkleizedInput {
  export const isMerkleizedDeposit = (input: MerkleizedInput): input is MerkleizedDeposit =>
    typeof input === "object" && input !== null && "input_from_party" in input;
  export const isMerkleizedChoice = (input: MerkleizedInput): input is MerkleizedChoice =>
    typeof input === "object" && input !== null && "for_choice_id" in input;
  export const isMerkleizedNotify = (input: MerkleizedInput): input is MerkleizedNotify =>
    typeof input === "object" && input !== null && "continuation_hash" in input;

  export const match = <T>(
    input: MerkleizedInput,
    handlers: {
      deposit: (v: MerkleizedDeposit) => T,
      choice: (v: MerkleizedChoice) => T,
      notify: (v: MerkleizedNotify) => T,
    },
  ): T =>
    isMerkleizedDeposit(input) ? handlers.deposit(input)
    : isMerkleizedChoice(input) ? handlers.choice(input)
    : handlers.notify(input);

  export const tryMatch = <T>(
    input: MerkleizedInput,
    handlers: {
      deposit?: (v: MerkleizedDeposit) => T,
      choice?: (v: MerkleizedChoice) => T,
      notify?: (v: MerkleizedNotify) => T,
    },
  ): Result<T, string> =>
    isMerkleizedDeposit(input) ? handlers.deposit ? ok(handlers.deposit(input)) : err("Missing deposit handler")
    : isMerkleizedChoice(input) ? handlers.choice ? ok(handlers.choice(input)) : err("Missing choice handler")
    : handlers.notify ? ok(handlers.notify(input)) : err("Missing notify handler");

  export const jsonCodec: JsonCodec<MerkleizedInput> = fromCodecThunkFn(() =>
    altJsonCodecs(
      [MerkleizedDeposit.jsonCodec, MerkleizedChoice.jsonCodec, MerkleizedNotify.jsonCodec],
      (serDeposit, serChoice, serNotify) => (input: MerkleizedInput) => match(input, {
        deposit: serDeposit,
        choice: serChoice,
        notify: serNotify,
      })
    )
  );
  export const areEqual = (a: MerkleizedInput, b: MerkleizedInput): boolean =>
    isMerkleizedDeposit(a) ? isMerkleizedDeposit(b) && MerkleizedDeposit.areEqual(a, b)
    : isMerkleizedChoice(a) ? isMerkleizedChoice(b) && MerkleizedChoice.areEqual(a, b)
    : isMerkleizedNotify(b) && MerkleizedNotify.areEqual(a, b);
}

export type Input = NormalInput | MerkleizedInput;
export namespace Input {
  export const isNormal = (input: Input): input is NormalInput =>
    typeof input === "string" || (typeof input === "object" && input !== null && !("continuation_hash" in input));
  export const isMerkleized = (input: Input): input is MerkleizedInput =>
    typeof input === "object" && input !== null && "continuation_hash" in input;

  export const match = <T>(
    input: Input,
    handlers: {
      normal: (v: NormalInput) => T,
      merkleized: (v: MerkleizedInput) => T,
    },
  ): T =>
    isMerkleized(input) ? handlers.merkleized(input) : handlers.normal(input);

  export const tryMatch = <T>(
    input: Input,
    handlers: {
      normal?: (v: NormalInput) => T,
      merkleized?: (v: MerkleizedInput) => T,
    },
  ): Result<T, string> =>
    isMerkleized(input) ? handlers.merkleized ? ok(handlers.merkleized(input)) : err("Missing merkleized handler")
    : handlers.normal ? ok(handlers.normal(input)) : err("Missing normal handler");

  export const jsonCodec: JsonCodec<Input> = fromCodecThunkFn(() =>
    altJsonCodecs(
      [InputContent.jsonCodec, MerkleizedInput.jsonCodec],
      (serNormal, serMerkleized) => (input: Input) => match(input, {
        normal: serNormal,
        merkleized: serMerkleized,
      })
    )
  );
  export const areEqual = (a: Input, b: Input): boolean =>
    isMerkleized(a) ? isMerkleized(b) && MerkleizedInput.areEqual(a, b)
    : isMerkleized(b) ? false : InputContent.areEqual(a, b);
}
