# Web runtime. Does not require marlowe-indexer.service.
# It reads the same database the indexer writes; that database may be remote.
{ lib, pkgs, config, ... }:
let
  cfg = config.services.marlowe-runtime;
  inherit (lib) mkEnableOption mkOption mkIf types escapeShellArgs optionals;

  networkArgs =
    if cfg.networkMagic == null
    then [ "--mainnet" ]
    else [ "--testnet-magic" (toString cfg.networkMagic) ];
in
{
  options.services.marlowe-runtime = {
    database = {
      uri = mkOption {
        type = types.str;
        example = "postgresql://marlowe-runtime@db.example/marlowe-indexer";
        description = "Indexer database. Not required to be local.";
      };
    };
    enable = mkEnableOption "Marlowe runtime web server";
    networkMagic = mkOption {
      type = types.nullOr types.ints.unsigned;
      default = null;
    };
    openFirewall = mkOption {
      type = types.bool;
      default = false;
    };
    package = mkOption {
      type = types.package;
      description = "Package providing bin/marlowe-runtime.";
    };
    port = mkOption {
      type = types.port;
      default = 8090;
    };
    scriptRegistry = mkOption {
      type = types.nullOr types.path;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    users.users.marlowe-runtime = {
      isSystemUser = true;
      group = "marlowe-runtime";
    };
    users.groups.marlowe-runtime = { };

    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.port ];

    systemd.services.marlowe-runtime = {
      description = "Marlowe runtime";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      unitConfig.StartLimitIntervalSec = 0;
      serviceConfig = {
        User = "marlowe-runtime";
        Group = "marlowe-runtime";
        Restart = "always";
        RestartMaxDelaySec = "1h";
        RestartSteps = 10;
        ExecStart = escapeShellArgs (
          [
            "${cfg.package}/bin/marlowe-runtime"
            "--database-uri" cfg.database.uri
            "--port" (toString cfg.port)
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
