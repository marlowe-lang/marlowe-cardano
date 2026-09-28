# Imported from marlowe-plutus/flake.nix.
# Call: import ./nix/nixos/flake-outputs.nix { self = inputs.self; nixpkgs = inputs.nixpkgs; }
{ self, nixpkgs }:
let
  inherit (nixpkgs) lib;
  system = "x86_64-linux";
  stubFor = pkgs: pkgs.callPackage ./stub-indexer.nix { };

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

  testModules = pkgs: [
    self.nixosModules.indexer
    ./test-machine.nix
    {
      services.marlowe-indexer.package = stubFor pkgs;
      services.marlowe-indexer.database.sqitchDir = sqitchDir;
    }
  ];
in
{
  nixosModules.indexer = import ./indexer.nix;

  nixosConfigurations.indexer-test = nixpkgs.lib.nixosSystem {
    inherit system;
    modules = testModules nixpkgs.legacyPackages.${system};
  };

  forPkgs = pkgs: {
    packages.marlowe-indexer-stub = stubFor pkgs;
    checks.indexer-module = pkgs.testers.runNixOSTest {
      name = "marlowe-indexer-module";
      nodes.machine = { ... }: {
        imports = testModules pkgs;
      };
      testScript = ''
        machine.start()
        machine.wait_for_unit("postgresql.service")
        machine.wait_until_succeeds(
          "systemctl show -p Result --value marlowe-indexer-migrate.service | grep -q success"
        )
        machine.wait_for_unit("marlowe-indexer.service")
        machine.succeed("test -f /tmp/marlowe-indexer.started")
        machine.succeed("grep -q -- '--database-uri' /tmp/marlowe-indexer.started")
        machine.succeed("grep -q -- '--testnet-magic 1' /tmp/marlowe-indexer.started")
        machine.succeed("grep -q -- '--socket-path /run/cardano-node/node.socket' /tmp/marlowe-indexer.started")
        machine.succeed("grep -q -- 'user: marlowe-indexer' /tmp/marlowe-indexer.started")
        machine.succeed(
          "sudo -u postgres psql -d marlowe-indexer -Atc \"SELECT change FROM sqitch.changes WHERE change = 'schema'\""
        )
        machine.succeed(
          "sudo -u postgres psql -d marlowe-indexer -Atc \"SELECT change FROM sqitch.changes WHERE change = 'addEraHistory'\""
        )
      '';
    };
  };
}
