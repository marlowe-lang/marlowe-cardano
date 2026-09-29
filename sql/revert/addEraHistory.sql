BEGIN;

-- PostgreSQL does not support dropping a value from an enum, so we have to
-- recreate the type. Any rows that used the 'eraHistory' value cannot be
-- preserved across the revert.

DELETE FROM node_status WHERE attr::text = 'eraHistory';

ALTER TABLE node_status
    ALTER COLUMN attr TYPE text,
    ALTER COLUMN attr DROP DEFAULT;

DROP TYPE node_status_attr;

CREATE TYPE node_status_attr AS ENUM ('tip');

ALTER TABLE node_status
    ALTER COLUMN attr TYPE node_status_attr
        USING attr::node_status_attr;

COMMIT;
