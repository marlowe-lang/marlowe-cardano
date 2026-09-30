BEGIN;

CREATE TYPE marlowe.node_status_attr AS ENUM ('tip');

CREATE TABLE IF NOT EXISTS marlowe.node_status (
    attr marlowe.node_status_attr PRIMARY KEY,
    value bytea
);

COMMIT;
