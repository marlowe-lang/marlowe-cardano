import { altJsonCodecs, arrayOf, objectOf, type JsonCodec } from "@konduit/codec/json/codecs";
import { arrayAreEqualWith } from "../assoc-map.js";
import { Observation, Value } from "./value-and-observation.js";
import { Bound, ChoiceId } from "./choices.js";
import { Party } from "./participants.js";
import { AccountId } from "./payee.js";
import { Token } from "./token.js";
import { err, ok } from "neverthrow";
import type { Result } from "neverthrow";

export interface Choice {
  choose_between: Bound[];
  for_choice: ChoiceId;
}

export function Choice(bounds: Bound[], for_choice: ChoiceId): Choice {
  return { choose_between: bounds, for_choice };
}
export namespace Choice {
  export const jsonCodec: JsonCodec<Choice> = objectOf({
    choose_between: arrayOf(Bound.jsonCodec),
    for_choice: ChoiceId.jsonCodec,
  });
  export const areEqual = (a: Choice, b: Choice): boolean =>
    arrayAreEqualWith(a.choose_between, b.choose_between, Bound.areEqual) &&
    ChoiceId.areEqual(a.for_choice, b.for_choice);
}

export interface Deposit {
  party: Party;
  deposits: Value;
  of_token: Token;
  into_account: AccountId;
}

export function Deposit(
  party: Party,
  into_account: AccountId,
  of_token: Token,
  deposits: Value
): Deposit {
  return { party, into_account, of_token, deposits };
}
export namespace Deposit {
  export const jsonCodec: JsonCodec<Deposit> = objectOf({
    party: Party.jsonCodec,
    deposits: Value.jsonCodec,
    of_token: Token.jsonCodec,
    into_account: AccountId.jsonCodec,
  });
  export const areEqual = (a: Deposit, b: Deposit): boolean =>
    Party.areEqual(a.party, b.party) &&
    Value.areEqual(a.deposits, b.deposits) &&
    Token.areEqual(a.of_token, b.of_token) &&
    AccountId.areEqual(a.into_account, b.into_account);
}

export interface Notify {
  notify_if: Observation;
}

export function Notify(notify_if: Observation): Notify {
  return { notify_if };
}
export namespace Notify {
  export const jsonCodec: JsonCodec<Notify> = objectOf({
    notify_if: Observation.jsonCodec,
  });
  export const areEqual = (a: Notify, b: Notify): boolean =>
    Observation.areEqual(a.notify_if, b.notify_if);
}

export type Action = Deposit | Choice | Notify;

export namespace Action {
  export const isDeposit = (action: Action): action is Deposit =>
    typeof action === "object" && action !== null && "party" in action;
  export const isChoice = (action: Action): action is Choice =>
    typeof action === "object" && action !== null && "choose_between" in action;
  export const isNotify = (action: Action): action is Notify =>
    typeof action === "object" && action !== null && "notify_if" in action;

  export const match = <T>(
    action: Action,
    handlers: {
      deposit: (v: Deposit) => T,
      choice: (v: Choice) => T,
      notify: (v: Notify) => T,
    },
  ): T =>
    isDeposit(action) ? handlers.deposit(action)
    : isChoice(action) ? handlers.choice(action)
    : handlers.notify(action);

  export const tryMatch = <T>(
    action: Action,
    handlers: {
      deposit?: (v: Deposit) => T,
      choice?: (v: Choice) => T,
      notify?: (v: Notify) => T,
    },
  ): Result<T, string> =>
    isDeposit(action) ? handlers.deposit ? ok(handlers.deposit(action)) : err("Missing deposit handler")
    : isChoice(action) ? handlers.choice ? ok(handlers.choice(action)) : err("Missing choice handler")
    : handlers.notify ? ok(handlers.notify(action)) : err("Missing notify handler")

  export const jsonCodec: JsonCodec<Action> = altJsonCodecs(
    [Deposit.jsonCodec, Choice.jsonCodec, Notify.jsonCodec],
    (serDeposit, serChoice, serNotify) => (action: Action) => match(action, {
      deposit: serDeposit,
      choice: serChoice,
      notify: serNotify,
    })
  );
  export const areEqual = (a: Action, b: Action): boolean =>
    isDeposit(a) ? isDeposit(b) && Deposit.areEqual(a, b)
    : isChoice(a) ? isChoice(b) && Choice.areEqual(a, b)
    : isNotify(b) && Notify.areEqual(a, b);
}
