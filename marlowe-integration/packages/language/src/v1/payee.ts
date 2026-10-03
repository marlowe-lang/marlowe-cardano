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
  // `AccountId` is currently a `Party` alias. The helper is wired up so
  // that moving to a tagged/branded representation later only requires
  // swapping the body here.
  export const areEqual = (a: AccountId, b: AccountId): boolean => Party.areEqual(a, b);
}

export function PayeeAccount(account: AccountId): PayeeAccount {
  return { account };
}
export namespace PayeeAccount {
  export const jsonCodec: JsonCodec<PayeeAccount> = objectOf({
    account: AccountId.jsonCodec,
  });
  export const areEqual = (a: PayeeAccount, b: PayeeAccount): boolean =>
    AccountId.areEqual(a.account, b.account);
}

export function PayeeParty(party: AccountId): PayeeParty {
  return { party };
}
export namespace PayeeParty {
  export const jsonCodec: JsonCodec<PayeeParty> = objectOf({
    party: AccountId.jsonCodec,
  });
  export const areEqual = (a: PayeeParty, b: PayeeParty): boolean =>
    AccountId.areEqual(a.party, b.party);
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
  export const areEqual = (a: Payee, b: Payee): boolean => {
    if ("account" in a && "account" in b) {
      return PayeeAccount.areEqual(a, b);
    }
    if ("party" in a && "party" in b) {
      return PayeeParty.areEqual(a, b);
    }
    return false;
  };
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