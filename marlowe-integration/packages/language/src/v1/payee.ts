import { altJsonCodecs, objectOf, type JsonCodec } from "@konduit/codec/json/codecs";
import type { Party } from "./participants.js";
import { Party as PartyNamespace } from "./participants.js";

export type AccountId = Party;

export namespace AccountId {
  export const jsonCodec: JsonCodec<AccountId> = PartyNamespace.jsonCodec;
}

export interface PayeeAccount {
  account: AccountId;
}

export namespace PayeeAccount {
  export const jsonCodec: JsonCodec<PayeeAccount> = objectOf({
    account: AccountId.jsonCodec,
  });
}

export interface PayeeParty {
  party: AccountId;
}

export namespace PayeeParty {
  export const jsonCodec: JsonCodec<PayeeParty> = objectOf({
    party: AccountId.jsonCodec,
  });
}

export type Payee = PayeeAccount | PayeeParty;

export namespace Payee {
  export const jsonCodec: JsonCodec<Payee> = altJsonCodecs(
    [PayeeAccount.jsonCodec, PayeeParty.jsonCodec],
    (serAccount, serParty) => (payee: Payee) =>
      "account" in payee ? serAccount(payee) : serParty(payee)
  );
}

export type PayeeMatcher<T> = {
  party: (party: Party) => T;
  account: (account: AccountId) => T;
};

export function matchPayee<T>(matcher: PayeeMatcher<T>): (payee: Payee) => T;
export function matchPayee<T>(matcher: Partial<PayeeMatcher<T>>): (payee: Payee) => T | undefined;
export function matchPayee<T>(matcher: Partial<PayeeMatcher<T>>) {
  return (payee: Payee) => {
    if ("party" in payee && matcher.party) {
      return matcher.party(payee.party);
    } else if ("account" in payee && matcher.account) {
      return matcher.account(payee.account);
    }
  };
}
