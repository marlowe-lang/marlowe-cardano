# One guest: preprod node on the host snapshot, postgres, indexer on the node socket.
# Import next to inputs.cardano-node.nixosModules.cardano-node.
# hostSnapshot must exist and contain immutable/. QEMU will not start otherwise.
{ hostSnapshot }:
{ config, lib, pkgs, ... }:
{
  system.stateVersion = "25.11";
  environment.systemPackages = [ pkgs.postgresql pkgs.sqitchPg ];

  # password = "" does not unlock the account, and build-vm may not apply it.
  services.getty.autologinUser = "root";
  users.mutableUsers = false;
  users.users.root.password = "root";

  # These options exist only in the build-vm submodule, not on a host config.
  virtualisation.vmVariant.virtualisation = {
    memorySize = 4096;
    sharedDirectories.preprod-db = {
      source = hostSnapshot;
      target = "/mnt/preprod-db";
    };
  };

  services.cardano-node = {
    enable = true;
    environment = "preprod";
    databasePath = "/mnt/preprod-db";
    socketPath = _: "/run/cardano-node/node.socket";
  };

  services.postgresql = {
    enable = true;
    ensureDatabases = [ "marlowe-indexer" ];
    ensureUsers = [{
      name = "marlowe-indexer";
      ensureDBOwnership = true;
    }];
    identMap = ''
      indexer-map marlowe-indexer marlowe-indexer
    '';
    authentication = lib.mkOverride 10 ''
      local all postgres                    peer
      local marlowe-indexer marlowe-indexer peer map=indexer-map
      host  all all 127.0.0.1/32            md5
      host  all all ::1/128                 md5
    '';
  };

  services.marlowe-indexer = {
    enable = true;
    networkMagic = 1;
    socketPath = "/run/cardano-node/node.socket";
    # This group is created by the cardano-node service.
    # The string literal is hard coded and not accessible from here.
    socketGroup = "cardano-node";
    database.name = "marlowe-indexer";
  };
}
