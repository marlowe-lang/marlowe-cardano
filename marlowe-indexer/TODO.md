# Cardano Era History in DB

The goal of this task is to extend the cardano node related state so we include era history information in the database. There is an existing structure for that status property and we can follow the same pattern.

## The details

* We use PostgreSQL as DB engine and sqitch for the database migrations.

* Please check the previous migrations which can be found in `sql/deploy`, `sql/revert` and `sql/verify` folders. Please focus on the migration which added the `status` table and `tip` info to that table. Please add a new property `eraHistory` and use a binary blob for the data type.

* Please check the `marlowe-indexer/db` package which contains insertion API for the previous `tip` status. Please follow the same pattern and add a new insertion API for the `eraHistory` property.

* Please check the insertion flow in the `marlowe-indexer/src` node follower code (the persistence layer). Can we easily extend that flow to include the new `eraHistory` property? If not, please propose a solution and ask for feedback.


## Final decisions

* Actually we can include the `eraHistory` query in the marlowe follower component - it is poked on every block anyway and contains the query ability. Let's not modify node follower etc.

* Please use CBOR encoding for the `eraHistory` from Cardano.API if available. In the case of our previous node tip encoding we used custom binary format because we used our own domain level type to represent the tip itself. We don't have and don't need a domain level type for the era history, so we can use the CBOR encoding from Cardano.API directly.

## Testing

* There is a database running in my test environment on port 15432. Please test the migration.

* Please leave the final testing of the indexer for me.
