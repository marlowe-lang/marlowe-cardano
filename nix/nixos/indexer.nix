{ lib, pkgs, config, ... }:
let
  cfg = config.services.marlowe-indexer;
  inherit (lib) mkEnableOption mkOption mkIf types escapeShellArgs optionals;

  networkArgs =
    if cfg.networkMagic == null
    then [ "--mainnet" ]
    else [ "--testnet-magic" (toString cfg.networkMagic) ];

  dbUri = "postgresql://${cfg.database.user}@/${cfg.database.name}";
  sqitchTarget = "db:pg://${cfg.database.user}@/${cfg.database.name}";

  sqitchUserConfig = pkgs.writeText "sqitch.conf" ''
    [user]
      name = ${cfg.database.user}
      email = ${cfg.database.user}@localhost
  '';

  migrate = pkgs.writeShellApplication {
    name = "marlowe-indexer-migrate";
    runtimeInputs = [ pkgs.sqitchPg pkgs.postgresql ];
    text = ''
      export SQITCH_USER_CONFIG=${sqitchUserConfig}
      export TZ=Etc/UTC
      export PGUSER=${cfg.database.user}
      export PGDATABASE=${cfg.database.name}
      cd ${cfg.database.sqitchDir}
      sqitch deploy --target ${sqitchTarget}
      ${lib.concatMapStrings (role: ''
        psql -v ON_ERROR_STOP=1 -c ${lib.escapeShellArg ''
          GRANT USAGE ON SCHEMA marlowe TO "${role}";
          GRANT SELECT ON ALL TABLES IN SCHEMA marlowe TO "${role}";
          ALTER DEFAULT PRIVILEGES IN SCHEMA marlowe GRANT SELECT ON TABLES TO "${role}";
        ''}
      '') cfg.database.readers}
    '';
  };
in
{
  options.services.marlowe-indexer = {
    enable = mkEnableOption "Marlowe indexer";
    package = mkOption {
      type = types.package;
      description = "Package providing bin/marlowe-indexer.";
    };
    socketPath = mkOption {
      type = types.str;
      description = "Cardano node socket. Set to the node service socket.";
    };
    socketGroup = mkOption {
      type = types.str;
      default = "cardano-node";
      description = "Group that may connect to the node socket. Indexer user is added to it.";
    };
    cardanoCli = mkOption {
      type = types.package;
      description = "cardano-cli used to poll query tip before the indexer starts.";
    };
    networkMagic = mkOption {
      type = types.nullOr types.ints.unsigned;
      default = null;
    };
    scriptRegistry = mkOption {
      type = types.nullOr types.path;
      default = null;
    };
    database = {
      name = mkOption { type = types.str; default = "marlowe"; };
      user = mkOption { type = types.str; default = "marlowe-indexer"; };
      migrate = mkOption { type = types.bool; default = true; };
      sqitchDir = mkOption { type = types.nullOr types.path; default = null; };
      readers = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Roles granted SELECT on marlowe after Sqitch. Empty if the runtime is elsewhere.";
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = !cfg.database.migrate || cfg.database.sqitchDir != null;
      message = "services.marlowe-indexer.database.sqitchDir is required when migrate = true.";
    }];

    users.users.${cfg.database.user} = {
      isSystemUser = true;
      group = cfg.database.user;
      extraGroups = [ cfg.socketGroup ];
    };
    users.groups.${cfg.database.user} = { };

    # cardano-node is Type=simple and does not sd_notify. This oneshot is the gate.
    systemd.services.cardano-node-ready = {
      description = "Wait until cardano-cli query tip succeeds";
      after = [ "cardano-node.service" ];
      bindsTo = [ "cardano-node.service" ];
      path = [ cfg.cardanoCli ];
      environment = {
        CARDANO_NODE_SOCKET_PATH = cfg.socketPath;
      } // lib.optionalAttrs (cfg.networkMagic != null) {
        CARDANO_NODE_NETWORK_ID = toString cfg.networkMagic;
      };
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "cardano-node-ready" ''
          set -euo pipefail
          for _ in $(seq 1 600); do
            if cardano-cli query tip ${
              if cfg.networkMagic == null
              then "--mainnet"
              else "--testnet-magic ${toString cfg.networkMagic}"
            }; then
              exit 0
            fi
            sleep 1
          done
          echo "cardano-cli query tip did not succeed" >&2
          exit 1
        '';
      };
    };

    systemd.services.marlowe-indexer-migrate = mkIf cfg.database.migrate {
      description = "Marlowe indexer sqitch deploy";
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];
      serviceConfig = {
        Type = "oneshot";
        User = cfg.database.user;
        Group = cfg.database.user;
        ExecStart = "${migrate}/bin/marlowe-indexer-migrate";
      };
    };

    systemd.services.marlowe-indexer = {
      description = "Marlowe indexer";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" "postgresql.service" "cardano-node-ready.service" ]
        ++ optionals cfg.database.migrate [ "marlowe-indexer-migrate.service" ];
      requires = [ "postgresql.service" "cardano-node-ready.service" ]
        ++ optionals cfg.database.migrate [ "marlowe-indexer-migrate.service" ];
      unitConfig.StartLimitIntervalSec = 0;
      serviceConfig = {
        User = cfg.database.user;
        Group = cfg.database.user;
        SupplementaryGroups = [ cfg.socketGroup ];
        Restart = "always";
        RestartMaxDelaySec = "1h";
        RestartSteps = 10;
        ExecStart = escapeShellArgs (
          [
            "${cfg.package}/bin/marlowe-indexer"
            "--database-uri" dbUri
            "--socket-path" cfg.socketPath
          ]
          ++ networkArgs
          ++ optionals (cfg.scriptRegistry != null) [
            "--script-registry" (toString cfg.scriptRegistry)
          ]
        );
      };
    };
  };
}
