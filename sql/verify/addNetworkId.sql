BEGIN;

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_enum e
    JOIN pg_type t ON e.enumtypid = t.oid
    JOIN pg_namespace n ON n.oid = t.typnamespace
    WHERE
      t.typname = 'node_status_attr'
      AND n.nspname = 'marlowe'
      AND e.enumlabel = 'networkId'
  ) THEN
    RAISE EXCEPTION 'Enum value "networkId" not present in type "marlowe.node_status_attr"';
  END IF;
END;
$$;

ROLLBACK;
