BEGIN;

CREATE TYPE marlowe.indexer_status_attr AS ENUM ('tip');

CREATE TABLE IF NOT EXISTS marlowe.indexer_status (
    attr marlowe.indexer_status_attr PRIMARY KEY,
    value bytea
);

COMMIT;
