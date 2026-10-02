/**
 * This module defines the participant types as defined in the Marlowe Specification.
 * @see Section 2.1.1 of the {@link https://github.com/input-output-hk/marlowe/releases/download/v3/specification-v3.pdf | Marlowe Specification}
 * @packageDocumentation
 */
import type { Sort } from "../assoc-map.js";
import { altJsonCodecs, json2StringCodec, objectOf, type JsonCodec } from "@konduit/codec/json/codecs";
import { AddressBech32 } from "./address.js";

// Types ---------------------------------------------------------------------

export type Address = {
  address: AddressBech32;
};

export type RoleName = string;

export type Role = {
  role_token: RoleName;
};

export type Party = Address | Role;

// Codecs + smart constructors ------------------------------------------------

export function Address(address: AddressBech32): Address {
  return { address };
}
export namespace Address {
  export const jsonCodec: JsonCodec<Address> = objectOf({
    address: AddressBech32.jsonCodec,
  });
}

export namespace RoleName {
  export const jsonCodec: JsonCodec<RoleName> = json2StringCodec;
}

export function Role(roleToken: RoleName): Role {
  return { role_token: roleToken };
}
export namespace Role {
  export const jsonCodec: JsonCodec<Role> = objectOf({
    role_token: RoleName.jsonCodec,
  });
}

export function Party(party: Role | Address): Party {
  return party;
}
export namespace Party {
  export const jsonCodec: JsonCodec<Party> = altJsonCodecs(
    [Address.jsonCodec, Role.jsonCodec],
    (serAddress, serRole) => (party: Party) =>
      "address" in party ? serAddress(party) : serRole(party)
  );
}

// Helpers --------------------------------------------------------------------

export const partiesToStrings: (parties: Party[]) => string[] = (parties) =>
  parties.map(partyToString);

export const partyToString: (party: Party) => string = (party) =>
  "role_token" in party ? party.role_token : party.address;

export function partyCmp(a: Party, b: Party): Sort {
  if ("role_token" in a && !("role_token" in b)) {
    return "LowerThan";
  }
  if (!("role_token" in a) && "role_token" in b) {
    return "GreaterThan";
  }
  if ("address" in a && "address" in b) {
    return strCmp(a.address, b.address);
  }
  if ("role_token" in a && "role_token" in b) {
    return strCmp(a.role_token, b.role_token);
  }
  throw new Error("Unreachable");
}

function strCmp(a: string, b: string): Sort {
  if (a < b) {
    return "LowerThan";
  }
  if (a > b) {
    return "GreaterThan";
  }
  return "EqualTo";
}