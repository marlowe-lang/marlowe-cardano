# Postgres + dummy node socket. Package is set by flake-outputs.nix.
{ lib, pkgs, ... }:
{
  system.stateVersion = "25.11";

  services.postgresql = {
    enable = true;
    # ensureDBOwnership requires a database with the same name as the user.
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

  environment.systemPackages = [ pkgs.postgresql pkgs.sqitchPg ];

  systemd.tmpfiles.rules = [
    "d /run/cardano-node 0755 root root -"
    "f /run/cardano-node/node.socket 0644 root root -"
  ];

  services.marlowe-indexer = {
    enable = true;
    networkMagic = 1;
    socketPath = "/run/cardano-node/node.socket";
    database.name = "marlowe-indexer";
  };
}
