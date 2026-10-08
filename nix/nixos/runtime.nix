# Web runtime. Does not require marlowe-indexer.service.
# It reads the same database the indexer writes; that database may be remote.
{ lib, pkgs, config, ... }:
let
  cfg = config.services.marlowe-runtime;
  inherit (lib) mkEnableOption mkOption mkIf types escapeShellArgs optionals literalExpression;

  networkArgs =
    if cfg.networkMagic == null
    then [ "--mainnet" ]
    else [ "--testnet-magic" (toString cfg.networkMagic) ];

  storeArgs =
    if cfg.store.backend == "in-memory" then
      [ "--in-memory-store" ]
    else
      [ "--store-dir" cfg.store.directory ]
      ++ optionals (cfg.store.maxContractAge != null) [
        "--max-contract-age" (toString cfg.store.maxContractAge)
      ]
      ++ optionals (cfg.store.maxStoreSize != null) [
        "--max-store-size" (toString cfg.store.maxStoreSize)
      ];
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
    store = {
      backend = mkOption {
        type = types.enum [ "filesystem" "in-memory" ];
        default = "filesystem";
        description = ''
          Contract store backend.
          `"in-memory"` is non-persistent and intended only for debug or
          development environments — contract data does not survive
          restarts. `"filesystem"` persists contracts under
          `store.directory`.
        '';
      };
      directory = mkOption {
        type = types.path;
        default = "/var/lib/marlowe-runtime/store";
        defaultText = literalExpression "\"/var/lib/marlowe-runtime/store\"";
        description = ''
          Directory used to persist the contract store. Only meaningful
          with `store.backend = "filesystem"` — its value is ignored
          when using the "in-memory" backend. Created automatically with
          ownership matching the `marlowe-runtime` user and mode `0750`.
        '';
      };
      maxContractAge = mkOption {
        type = types.nullOr types.ints.unsigned;
        default = null;
        description = ''
          Maximum age, in seconds, a contract may remain in the store
          before becoming eligible for garbage collection. Only
          meaningful with `store.backend = "filesystem"`. If `null`,
          the server default is used.
        '';
      };
      maxStoreSize = mkOption {
        type = types.nullOr types.ints.unsigned;
        default = null;
        description = ''
          Maximum allowed size of the contract store, in bytes. Only
          meaningful with `store.backend = "filesystem"`. If `null`,
          the server default is used.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.store.backend == "filesystem" || (cfg.store.maxContractAge == null && cfg.store.maxStoreSize == null);
        message = ''
          services.marlowe-runtime.store.maxContractAge and store.maxStoreSize
          are only meaningful with store.backend = "filesystem"; leave them
          unset (or set to null) when using the "in-memory" backend.
        '';
      }
    ];

    users.users.marlowe-runtime = {
      isSystemUser = true;
      group = "marlowe-runtime";
    };
    users.groups.marlowe-runtime = { };

    systemd.tmpfiles.rules = mkIf (cfg.store.backend == "filesystem") [
      "d ${toString cfg.store.directory} 0750 marlowe-runtime marlowe-runtime - -"
    ];

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
          ++ storeArgs
        );
      };
    };
  };
}
