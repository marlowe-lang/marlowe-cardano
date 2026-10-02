import { altJsonCodecs, arrayOf, objectOf, type JsonCodec } from "@konduit/codec/json/codecs";
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
}
