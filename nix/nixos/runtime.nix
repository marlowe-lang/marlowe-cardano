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
    enable = mkEnableOption "Marlowe runtime web server";
    package = mkOption {
      type = types.package;
      description = "Package providing bin/marlowe-runtime.";
    };
    socketPath = mkOption { type = types.str; };
    socketGroup = mkOption {
      type = types.str;
      default = "cardano-node";
    };
    networkMagic = mkOption {
      type = types.nullOr types.ints.unsigned;
      default = null;
    };
    port = mkOption {
      type = types.port;
      default = 8090;
    };
    openFirewall = mkOption {
      type = types.bool;
      default = false;
    };
    scriptRegistry = mkOption {
      type = types.nullOr types.path;
      default = null;
    };
    database = {
      uri = mkOption {
        type = types.str;
        example = "postgresql://marlowe-runtime@db.example/marlowe-indexer";
        description = "Indexer database. Not required to be local.";
      };
    };
  };

  config = mkIf cfg.enable {
    users.users.marlowe-runtime = {
      isSystemUser = true;
      group = "marlowe-runtime";
      extraGroups = [ cfg.socketGroup ];
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
        SupplementaryGroups = [ cfg.socketGroup ];
        Restart = "always";
        RestartMaxDelaySec = "1h";
        RestartSteps = 10;
        ExecStart = escapeShellArgs (
          [
            "${cfg.package}/bin/marlowe-runtime"
            "--database-uri" cfg.database.uri
            "--socket-path" cfg.socketPath
            "--port" (toString cfg.port)
          ]
          ++ networkArgs
          ++ optionals (cfg.scriptRegistry != null) [
            "--scripts-registry-file" (toString cfg.scriptRegistry)
          ]
        );
      };
    };
  };
}
