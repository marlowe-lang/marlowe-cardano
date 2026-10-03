import { altJsonCodecs, arrayOf, objectOf, type JsonCodec } from "@konduit/codec/json/codecs";
import { arrayAreEqualWith } from "../assoc-map.js";
import { Observation, Value } from "./value-and-observation.js";
import { Bound, ChoiceId } from "./choices.js";
import { Party } from "./participants.js";
import { AccountId } from "./payee.js";
import { Token } from "./token.js";

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
  export const jsonCodec: JsonCodec<Action> = altJsonCodecs(
    [Deposit.jsonCodec, Choice.jsonCodec, Notify.jsonCodec],
    (serDeposit, serChoice, serNotify) => (action: Action) => {
      if ("party" in action) {
        return serDeposit(action);
      }
      if ("choose_between" in action) {
        return serChoice(action);
      }
      // notify_if
      return serNotify(action);
    }
  );
  export const areEqual = (a: Action, b: Action): boolean => {
    if ("party" in a) {
      return "party" in b && Deposit.areEqual(a, b);
    }
    if ("choose_between" in a) {
      return "choose_between" in b && Choice.areEqual(a, b);
    }
    return "notify_if" in b && Notify.areEqual(a, b);
  };
}
