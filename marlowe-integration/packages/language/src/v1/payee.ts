import { altJsonCodecs, objectOf, type JsonCodec } from "@konduit/codec/json/codecs";
import { err, ok } from "neverthrow";
import type { Result } from "neverthrow";
import { Party } from "./participants.js";

// Types ---------------------------------------------------------------------

export type AccountId = Party;
export function AccountId(party: Party): AccountId {
  return party;
}
export namespace AccountId {
  export const jsonCodec: JsonCodec<AccountId> = Party.jsonCodec;
  // `AccountId` is currently a `Party` alias. The helper is wired up so
  // that moving to a tagged/branded representation later only requires
  // swapping the body here.
  export const areEqual = (a: AccountId, b: AccountId): boolean => Party.areEqual(a, b);
}

export type PayeeAccount = {
  account: AccountId;
};
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

export type PayeeParty = {
  party: AccountId;
};
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

export type Payee = PayeeAccount | PayeeParty;
export function Payee(p: Party): Payee {
  return { party: p };
}
export namespace Payee {
  export const isPayeeAccount = (payee: Payee): payee is PayeeAccount =>
    typeof payee === "object" && payee !== null && "account" in payee;
  export const isPayeeParty = (payee: Payee): payee is PayeeParty =>
    typeof payee === "object" && payee !== null && "party" in payee;

  export const match = <T>(
    payee: Payee,
    handlers: {
      account: (v: PayeeAccount) => T,
      party: (v: PayeeParty) => T,
    },
  ): T =>
    isPayeeAccount(payee) ? handlers.account(payee) : handlers.party(payee);

  export const tryMatch = <T>(
    payee: Payee,
    handlers: {
      account?: (v: PayeeAccount) => T,
      party?: (v: PayeeParty) => T,
    },
  ): Result<T, string> =>
    isPayeeAccount(payee) ? handlers.account ? ok(handlers.account(payee)) : err("Missing account handler")
    : handlers.party ? ok(handlers.party(payee)) : err("Missing party handler");

  export const jsonCodec: JsonCodec<Payee> = altJsonCodecs(
    [PayeeAccount.jsonCodec, PayeeParty.jsonCodec],
    (serAccount, serParty) => (payee: Payee) => match(payee, {
      account: serAccount,
      party: serParty,
    })
  );
  export const areEqual = (a: Payee, b: Payee): boolean =>
    isPayeeAccount(a) ? isPayeeAccount(b) && PayeeAccount.areEqual(a, b)
    : isPayeeParty(b) && PayeeParty.areEqual(a, b);
}
