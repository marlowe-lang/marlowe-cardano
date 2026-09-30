# Mithril restore before cardano-node.service. No-op if db/immutable exists.
# Import next to inputs.cardano-node.nixosModules.cardano-node.
#
# A local snapshot is not this module. Point the node at it:
#   services.cardano-node.databasePath = "/mnt/preprod-db";
#   virtualisation.sharedDirectories.preprod-db.source = "/abs/host/db";
#   virtualisation.sharedDirectories.preprod-db.target = "/mnt/preprod-db";
{ config, lib, pkgs, ... }:
let
  cfg = config.services.cardano-node-bootstrap;
  networks = {
    preprod = {
      aggregator = "https://aggregator.release-preprod.api.mithril.network/aggregator";
      genesisVkey = "https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey";
      ancillaryVkey = "https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/release-preprod/ancillary.vkey";
    };
    preview = {
      aggregator = "https://aggregator.pre-release-preview.api.mithril.network/aggregator";
      genesisVkey = "https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey";
      ancillaryVkey = "https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/pre-release-preview/ancillary.vkey";
    };
    mainnet = {
      aggregator = "https://aggregator.release-mainnet.api.mithril.network/aggregator";
      genesisVkey = "https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey";
      ancillaryVkey = "https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/release-mainnet/ancillary.vkey";
    };
  };
  net = networks.${cfg.network};
  db = cfg.databasePath;
in
{
  options.services.cardano-node-bootstrap = {
    enable = lib.mkEnableOption "Mithril bootstrap for cardano-node";
    network = lib.mkOption {
      type = lib.types.enum [ "preprod" "preview" "mainnet" ];
      default = "preprod";
    };
    databasePath = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/cardano-node/db";
    };
    mithrilClient = lib.mkOption { type = lib.types.package; };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.cardano-node-bootstrap = {
      description = "Mithril bootstrap (${cfg.network})";
      wantedBy = [ "multi-user.target" ];
      before = [ "cardano-node.service" ];
      requiredBy = [ "cardano-node.service" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      path = [ pkgs.coreutils pkgs.curl cfg.mithrilClient ];
      serviceConfig.Type = "oneshot";
      script = ''
        set -euo pipefail
        if [ -d ${db}/immutable ]; then
          echo "db present, skipping mithril"
          exit 0
        fi
        mkdir -p ${dirOf db}
        export AGGREGATOR_ENDPOINT=${net.aggregator}
        export GENESIS_VERIFICATION_KEY="$(curl -fsSL ${net.genesisVkey})"
        export ANCILLARY_VERIFICATION_KEY="$(curl -fsSL ${net.ancillaryVkey})"
        mithril-client cardano-db download \
          --include-ancillary \
          --download-dir ${dirOf db} \
          latest
      '';
    };

    systemd.services.cardano-node = {
      after = [ "cardano-node-bootstrap.service" ];
      requires = [ "cardano-node-bootstrap.service" ];
    };
  };
}
