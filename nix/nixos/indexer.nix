{ lib, pkgs, config, ... }:
let
  cfg = config.services.marlowe-indexer;
  inherit (lib)
    mkEnableOption mkOption mkIf mkMerge types
    escapeShellArgs optionals;

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
    enable = mkEnableOption "Marlowe indexer";

    package = mkOption {
      type = types.package;
      description = "Package providing bin/marlowe-indexer.";
    };

    socketPath = mkOption {
      type = types.str;
      description = "Cardano node socket path (runtime path, not a store path).";
    };

    networkMagic = mkOption {
      type = types.nullOr types.ints.unsigned;
      default = null;
      description = ''
        Network magic. null → --mainnet.
        Preprod = 1, preview = 2, custom = any u32.
      '';
    };

    scriptRegistry = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Optional script-registry JSON (copied into the store).";
    };

    database = {
      name = mkOption {
        type = types.str;
        default = "marlowe";
      };
      user = mkOption {
        type = types.str;
        default = "marlowe-indexer";
      };
      migrate = mkOption {
        type = types.bool;
        default = true;
        description = "Run sqitch deploy before starting the indexer.";
      };
      sqitchDir = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = "Directory containing sqitch.plan, deploy/, revert/, verify/.";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      assertions = [{
        assertion = !cfg.database.migrate || cfg.database.sqitchDir != null;
        message = "services.marlowe-indexer.database.sqitchDir must be set when migrate = true.";
      }];

      users.users.${cfg.database.user} = {
        isSystemUser = true;
        group = cfg.database.user;
      };
      users.groups.${cfg.database.user} = { };

      systemd.services.marlowe-indexer = {
        description = "Marlowe indexer";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "postgresql.service" ]
          ++ lib.optional cfg.database.migrate "marlowe-indexer-migrate.service";
        requires = [ "postgresql.service" ]
          ++ lib.optional cfg.database.migrate "marlowe-indexer-migrate.service";
        unitConfig.StartLimitIntervalSec = 0;
        serviceConfig = {
          User = cfg.database.user;
          Group = cfg.database.user;
          Restart = "always";
          RestartMaxDelaySec = "1h";
          RestartSteps = 10;
          ExecStart = escapeShellArgs (
            [
              "${cfg.package}/bin/marlowe-indexer"
              "--database-uri"
              dbUri
              "--socket-path"
              (toString cfg.socketPath)
            ]
            ++ networkArgs
            ++ optionals (cfg.scriptRegistry != null) [
              "--script-registry"
              (toString cfg.scriptRegistry)
            ]
          );
        };
      };
    }

    (mkIf cfg.database.migrate {
      systemd.services.marlowe-indexer-migrate = {
        description = "Marlowe indexer sqitch deploy";
        after = [ "postgresql.service" ];
        requires = [ "postgresql.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          User = cfg.database.user;
          Group = cfg.database.user;
          ExecStart = "${migrate}/bin/marlowe-indexer-migrate";
        };
      };
    })
  ]);
}
