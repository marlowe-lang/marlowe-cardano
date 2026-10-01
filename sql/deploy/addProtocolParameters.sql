BEGIN;

ALTER TYPE marlowe.node_status_attr ADD VALUE 'protocolParameters';

COMMIT;
