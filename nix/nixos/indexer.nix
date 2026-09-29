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
    '';
  };
in
{
  options.services.marlowe-indexer = {
    cardanoCli = mkOption {
      type = types.package;
      description = "cardano-cli used to poll query tip before the indexer starts.";
    };
    database = {
      name = mkOption { type = types.str; default = "marlowe"; };
      user = mkOption { type = types.str; default = "marlowe-indexer"; };
      migrate = mkOption { type = types.bool; default = true; };
      sqitchDir = mkOption { type = types.nullOr types.path; default = null; };
    };
    enable = mkEnableOption "Marlowe indexer";
    networkMagic = mkOption {
      type = types.nullOr types.ints.unsigned;
      default = null;
    };
    package = mkOption {
      type = types.package;
      description = "Package providing bin/marlowe-indexer.";
    };
    scriptRegistry = mkOption {
      type = types.nullOr types.path;
      default = null;
    };
    socketGroup = mkOption {
      type = types.str;
      default = "cardano-node";
      description = "Group that may connect to the node socket. Indexer user is added to it.";
    };
    socketPath = mkOption {
      type = types.str;
      description = "Cardano node socket. Set to the node service socket.";
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

    # Node creates the socket 0600. UMask makes new files 0660; the chmod
    # covers the socket the node has already created. Group, not world.
    systemd.services.cardano-node.serviceConfig = {
      UMask = lib.mkDefault "0007";
      RuntimeDirectoryMode = lib.mkDefault "0770";
    };

    systemd.services.cardano-node-ready = {
      description = "Wait until cardano-cli query tip succeeds";
      after = [ "cardano-node.service" ];
      bindsTo = [ "cardano-node.service" ];
      path = [ cfg.cardanoCli ];
      environment.CARDANO_NODE_SOCKET_PATH = cfg.socketPath;
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
