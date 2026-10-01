import type { Observation, Value } from "./value-and-observation.js";
import type { ChoiceId } from "./choices.js";
import type { Party } from "./participants.js";
import type { AccountId } from "./payee.js";
import type { Token } from "./token.js";
import type { Bound } from "./choices.js";

export interface Choice {
  choose_between: Bound[];
  for_choice: ChoiceId;
}

export interface Deposit {
  party: Party;
  deposits: Value;
  of_token: Token;
  into_account: AccountId;
}

export interface Notify {
  notify_if: Observation;
}

export type Action = Deposit | Choice | Notify;
