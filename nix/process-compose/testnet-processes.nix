{
  lib,
  coreutils,
  writeText,
  writeShellApplication,
  formats,
  cardonnay,
  cardano-node,
  cardano-cli,
}: let
  validate-testnet-env = writeShellApplication {
    name = "validate-testnet-env";
    runtimeInputs = [coreutils];
    # Check if all variables are defined and if not stop the process.
    text = ''
      set -euo pipefail
      set -x
      : "''${TESTNET_DIR:?}"
      : "''${CARDONNAY_TESTNET_ID:?}"
      : "''${CARDANO_NODE_NETWORK_ID:?}"
      : "''${CARDANO_NODE_SOCKET_PATH:?}"
    '';
  };

  clear-testnet-state = writeShellApplication {
    name = "clear-testnet-state";
    runtimeInputs = [coreutils];
    text = ''
      set -x
      if [ -d "$TESTNET_DIR" ]; then
        rm -rf "$TESTNET_DIR"
      fi
      mkdir -p "$TESTNET_DIR"
    '';
  };

  initialize-testnet = writeShellApplication {
    name = "initialize-testnet";
    runtimeInputs = [cardonnay cardano-node cardano-cli];
    text = ''
      set -x
      # If you want to experiment with Dijkstra protocol version please uncomment the line below and set the desired version.
      # export PROTOCOL_VERSION=12
      # That inspiration was taken from here:
      # https://github.com/IntersectMBO/cardonnay/blob/4c9f5396b41d9f959e3da4708690f0789547ddcc/src/cardonnay_scripts/scripts/common/common-start-fast#L236
      cardonnay create -t conway_fast -w "$TESTNET_DIR" -i "$CARDONNAY_TESTNET_ID"
    '';
  };

  set-faucet-info = writeShellApplication {
    name = "set-faucet-info";
    runtimeInputs = [coreutils];
    text = ''
      set -x
      cp "$(cardonnay inspect faucet -i 9 -w "$TESTNET_DIR" | jq -r .skey_file)" "$FAUCET_SKEY_FILE"
      cardonnay inspect faucet -i 9 -w "$TESTNET_DIR" | jq -r .address > "$FAUCET_ADDR_FILE"
    '';
  };

  # Readiness probe for `initialize-testnet`.
  #
  # The probe exits 0 only after the testnet has finished submitting its
  # configuration transaction (the custom tx that registers pools, CC members
  # and DReps). cardonnay waits `TX_SUBMISSION_DELAY` (≈60s) after the nodes
  # start before submitting it, so once `cardano-cli query tip` succeeds we
  # have a comfortable window to capture the initial UTxO snapshot and then
  # poll for a change.
  #
  # The script is stateful across invocations: the initial UTxO dump is
  # persisted to $TESTNET_DIR and reused on subsequent probes.
  await-testing-tx = writeShellApplication {
    name = "await-testing-tx";
    runtimeInputs = [cardano-cli coreutils];
    text = ''
      set -euo pipefail
      set -x

      : "''${TESTNET_DIR:?}"
      : "''${CARDANO_NODE_NETWORK_ID:?}"
      : "''${CARDANO_NODE_SOCKET_PATH:?}"

      INITIAL_UTXO_FILE="''${TESTNET_DIR}/initial-utxo.json"
      CURRENT_UTXO_FILE="''${TESTNET_DIR}/current-utxo.json"

      # Dump the current UTxO set. Exit 1 if the query fails (e.g. the node
      # is still synchronising) so the probe retries.
      cardano-cli query utxo \
        --testnet-magic "$CARDANO_NODE_NETWORK_ID" \
        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
        --whole-utxo > "$CURRENT_UTXO_FILE" 2>/dev/null || exit 1

      # First successful probe: snapshot the UTxO set so we can detect any
      # change on subsequent invocations.
      if [ ! -f "$INITIAL_UTXO_FILE" ]; then
        cp "$CURRENT_UTXO_FILE" "$INITIAL_UTXO_FILE" || exit 1
        exit 1
      fi

      # The UTxO set is unchanged - the configuration transaction has not
      # been submitted yet.
      if cmp -s "$INITIAL_UTXO_FILE" "$CURRENT_UTXO_FILE"; then
        exit 1
      fi

      echo "Testnet UTxO set has changed - configuration transaction detected."
      exit 0
    '';
  };
in {
  validate-testnet-env = {
    namespace = "testnet";
    command = "${validate-testnet-env}/bin/validate-testnet-env";
  };

  clear-testnet-state = {
    namespace = "testnet";
    command = "${clear-testnet-state}/bin/clear-testnet-state";
    depends_on = {
      validate-testnet-env.condition = "process_completed_successfully";
    };
  };

  initialize-testnet = {
    namespace = "testnet";
    command = "${initialize-testnet}/bin/initialize-testnet";
    depends_on = {
      validate-testnet-env.condition = "process_completed_successfully";
      clear-testnet-state.condition = "process_completed_successfully";
    };
    is_daemon = true;
    availability = {
      restart = "always";
    };
    shutdown = {
      command = "cardonnay control stop -i $CARDONNAY_TESTNET_ID -w $TESTNET_DIR";
      timeout_seconds = 15;
    };
    readiness_probe = {
      exec = {
        command = "${await-testing-tx}/bin/await-testing-tx";
      };
      initial_delay_seconds = 10;      # after we reduced the internal sleep
      period_seconds = 2;
      timeout_seconds = 5;
      success_threshold = 1;
      # 300 * 2s = 600s window; the configuration tx is submitted ~60s
      # after node start, but we keep a generous buffer.
      failure_threshold = 300;
    };
  };

  set-faucet-info = {
    namespace = "testnet";
    command = "${set-faucet-info}/bin/set-faucet-info";
    depends_on = {
      initialize-testnet.condition = "process_healthy";
    };
  };
}

