# The deployment guest. Not a service module: it returns a nixosSystem.
# Service modules stay in indexer.nix, runtime.nix and helpers like cardano-node-bootstrap.nix etc.
#
#   nixos.mkDeploymentTest {
#     cardanoNodeModule = inputs.cardano-node.nixosModules.cardano-node;
#     cardanoCli = inputs.cardano-node.packages.x86_64-linux.cardano-cli;
#     indexerPackage = inputs.self.packages.x86_64-linux.marlowe-indexer;
#     hostSnapshot = "/abs/path/to/preprod/db";
#   };
{ self, nixpkgs, system ? "x86_64-linux" }:
{ cardanoNodeModule, cardanoCli, indexerPackage, runtimePackage, hostSnapshot, hostUid }:
let
  inherit (nixpkgs) lib;
  dbName = "marlowe-indexer";
  # Currently we require and hard code preprod network for testing.
  networkMagic = 1;
  # This group is created by the cardano-node service.
  # The string literal is hard coded and not accessible from here.
  socketGroup = "cardano-node";
  socketPath = "/run/cardano-node/node.socket";
  sqitchDir = builtins.path {
    name = "marlowe-sqitch";
    path = "${self}/sql";
    filter = path: type:
      let p = toString path;
      in
        type == "directory"
        || lib.hasSuffix "/sqitch.plan" p
        || lib.hasSuffix "/sqitch.conf" p
        || lib.hasInfix "/deploy/" p
        || lib.hasInfix "/revert/" p
        || lib.hasInfix "/verify/" p;
  };
in
nixpkgs.lib.nixosSystem {
  inherit system;
  modules = [
    cardanoNodeModule
    (import ./indexer.nix)
    (import ./runtime.nix)
    ({ config, lib, pkgs, ... }: {
      environment.systemPackages = [ pkgs.postgresql pkgs.sqitchPg cardanoCli ];
      networking.enableIPv6 = false;
      services.getty.autologinUser = "root";
      system.stateVersion = "25.11";
      users.mutableUsers = false;
      users.users.root.password = "root";
      users.users.cardano-node = {
        uid = lib.mkForce hostUid;
        extraGroups = [ "users" ];
      };

      virtualisation.vmVariant.virtualisation = {
        forwardPorts = [
          { from = "host"; host.port = 18090; guest.port = 8090; }
        ];
        memorySize = 16 * 1024; # 16 GB
        sharedDirectories.preprod-db = {
          source = hostSnapshot;
          target = "/mnt/preprod-db";
        };
      };

      systemd.services.cardano-node.serviceConfig = {
        UMask = "0007";
        KillSignal = "SIGINT";
        TimeoutStopSec = "infinity"; # default ~90s then SIGKILL, which deletes the marker
      };

      services.cardano-node = {
        enable = true;
        environment = "preprod";
        databasePath = "/mnt/preprod-db";
        hostAddr = "0.0.0.0";
        socketPath = _: socketPath;
      };

      services.postgresql = {
        enable = true;
        ensureDatabases = [ dbName ];
        ensureUsers = [
          { name = "marlowe-indexer"; ensureDBOwnership = true; }
          { name = "marlowe-runtime"; }
        ];
        identMap = ''
          indexer-map marlowe-indexer marlowe-indexer
          indexer-map marlowe-runtime marlowe-runtime
        '';
        authentication = lib.mkOverride 10 ''
          local all postgres                    peer
          local marlowe-indexer marlowe-indexer peer map=indexer-map
          local marlowe-indexer marlowe-runtime peer map=indexer-map
          host  all all 127.0.0.1/32            md5
          host  all all ::1/128                 md5
        '';
      };

      services.marlowe-runtime = {
        enable = true;
        package = runtimePackage;
        networkMagic = networkMagic;
        openFirewall = true;
        port = 8090;
        database.uri = ''postgresql://marlowe-runtime@/${dbName}'';
        store.backend = "filesystem";
      };

      services.marlowe-indexer = {
        enable = true;
        package = indexerPackage;
        cardanoCli = cardanoCli;
        networkMagic = networkMagic;
        socketPath = socketPath;
        socketGroup = socketGroup;
        database.name = dbName;
        database.sqitchDir = sqitchDir;
        database.readers = [ "marlowe-runtime" ];
      };
    })
  ];
}
