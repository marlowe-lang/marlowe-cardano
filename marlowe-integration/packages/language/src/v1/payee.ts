import { altJsonCodecs, objectOf, type JsonCodec } from "@konduit/codec/json/codecs";
import { Party } from "./participants.js";

// Types ---------------------------------------------------------------------

export type AccountId = Party;

export type PayeeAccount = {
  account: AccountId;
};

export type PayeeParty = {
  party: AccountId;
};

export type Payee = PayeeAccount | PayeeParty;

// Codecs + smart constructors ------------------------------------------------

export namespace AccountId {
  export const jsonCodec: JsonCodec<AccountId> = Party.jsonCodec;
}

export function PayeeAccount(account: AccountId): PayeeAccount {
  return { account };
}
export namespace PayeeAccount {
  export const jsonCodec: JsonCodec<PayeeAccount> = objectOf({
    account: AccountId.jsonCodec,
  });
}

export function PayeeParty(party: AccountId): PayeeParty {
  return { party };
}
export namespace PayeeParty {
  export const jsonCodec: JsonCodec<PayeeParty> = objectOf({
    party: AccountId.jsonCodec,
  });
}

export function Payee(p: Party): Payee {
  return { party: p };
}
export namespace Payee {
  export const jsonCodec: JsonCodec<Payee> = altJsonCodecs(
    [PayeeAccount.jsonCodec, PayeeParty.jsonCodec],
    (serAccount, serParty) => (payee: Payee) =>
      "account" in payee ? serAccount(payee) : serParty(payee)
  );
}

// Helpers --------------------------------------------------------------------

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