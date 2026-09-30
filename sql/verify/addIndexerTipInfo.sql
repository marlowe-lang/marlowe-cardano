BEGIN;

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_type t
    JOIN pg_namespace n ON n.oid = t.typnamespace
    WHERE n.nspname = 'marlowe'
      AND t.typname = 'indexer_status_attr'
  ) THEN
    RAISE EXCEPTION 'Type "marlowe.indexer_status_attr" does not exist';
  END IF;

  IF NOT EXISTS (
    SELECT 1
    FROM information_schema.tables
    WHERE table_schema = 'marlowe'
      AND table_name = 'indexer_status'
  ) THEN
    RAISE EXCEPTION 'Table "marlowe.indexer_status" does not exist';
  END IF;
END;
$$;

ROLLBACK;
