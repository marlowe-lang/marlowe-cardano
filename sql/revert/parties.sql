-- Revert marlowe:parties from pg

BEGIN;

DROP TABLE marlowe.contractTxOutPartyAddress;
DROP TABLE marlowe.contractTxOutPartyRole;

COMMIT;
