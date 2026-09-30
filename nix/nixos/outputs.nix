# Imported from marlowe-plutus/flake.nix:
# nixos = import ./nix/nixos/outputs.nix { self = inputs.self; nixpkgs = inputs.nixpkgs; };
{ self, nixpkgs }:
let
  inherit (nixpkgs) lib;
  system = "x86_64-linux";
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
{
  nixosModules.marlowe-indexer = import ./indexer.nix;
  nixosModules.marlowe-runtime = import ./runtime.nix;
  nixosModules.cardano-node-bootstrap = import ./cardano-node-bootstrap.nix;

  mkDeploymentTest = import ./test-deployment.nix { inherit self nixpkgs; };
}

#   mkIndexerTest = { cardanoCli, cardanoNodeModule, hostSnapshot, indexerPackage }:
#     nixpkgs.lib.nixosSystem {
#       inherit system;
#       modules = [
#         cardanoNodeModule
#         self.nixosModules.indexer
# 
#         (import ./test-machine.nix { inherit hostSnapshot; })
#         {
#           environment.systemPackages = [ cardanoCli ];
#           services.marlowe-indexer = {
#             enable = true;
#             cardanoCli = cardanoCli;
#             package = indexerPackage;
#             database.sqitchDir = sqitchDir;
#           };
#         }
#       ];
#     };
# }
