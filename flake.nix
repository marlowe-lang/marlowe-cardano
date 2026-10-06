{
  description = "Marlowe Cardano implementation";


  inputs = {
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix?ref=2025.12.21";
    };

    std = {
      url = "github:divnix/std";
      inputs.n2c.follows = "n2c";
    };

    n2c.url = "github:nlewo/nix2container";

    # marlowe-plutus.url = "github:input-output-hk/marlowe-plutus";
    cardano-cli.url = "github:intersectmbo/cardano-cli?ref=cardano-cli-9.3.0.0";
    cardano-node.url = "github:input-output-hk/cardano-node?ref=9.1.1";
    cardano-addresses.url = "github:IntersectMBO/cardano-addresses?ref=3.12.0";

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = inputs: inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      # pkgs = import ./pkgs.nix { inherit inputs system; };
      repoRoot = ./.;
      pkgs =
        import inputs.nixpkgs {
          inherit system;
          config = inputs.haskell-nix.config;
          overlays = [
            inputs.iohk-nix.overlays.crypto
            inputs.iohk-nix.overlays.cardano-lib
            inputs.haskell-nix.overlay
            inputs.iohk-nix.overlays.haskell-nix-crypto
            inputs.iohk-nix.overlays.haskell-nix-extra
          ];
        };
      inherit (pkgs) lib;

      project = pkgs.haskell-nix.cabalProject' (
        { config, pkgs, ... }:
        {
          name = "marlowe-cardano";
          compiler-nix-name = lib.mkDefault "ghc984";
          src = lib.cleanSource ./.;
          inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };
          # modules = [{
          #   packages = {};
          # }];
          # flake.variants = {
          #   # ghc966 = {}; # Alias for the default variant
          #   # ghc984.compiler-nix-name = "ghc984";
          #   # ghc9102.compiler-nix-name = "ghc9102";
          #   # ghc9122.compiler-nix-name = "ghc9122";
          # };
        }
      );

      devShells.default = import ./nix/shell.nix {
        inherit inputs pkgs lib project system repoRoot;
      };

      # projectFlake = project.flake {};

      # defaultHydraJobs = {
      #   ghc966 = projectFlake.hydraJobs.ghc966;
      #   ghc984 = projectFlake.hydraJobs.ghc984;
      #   ghc9102 = projectFlake.hydraJobs.ghc9102;
      #   ghc9122 = projectFlake.hydraJobs.ghc9122;
      #   inherit packages;
      #   inherit devShells;
      #   required = utils.makeHydraRequiredJob hydraJobs;
      # };

      # hydraJobsPerSystem = {
      #   "x86_64-linux" = defaultHydraJobs;
      #   "x86_64-darwin" = defaultHydraJobs;
      #   "aarch64-linux" = defaultHydraJobs;
      #   "aarch64-darwin" = defaultHydraJobs;
      # };
      # hydraJobs = utils.flattenDerivationTree "-" hydraJobsPerSystem.${system};
    in
    {
      # inherit packages;
      inherit devShells;
      # inherit hydraJobs;
    }
  );



  # outputs = inputs@{ self, nixpkgs, ... }: inputs.iogx.lib.mkFlake {
  #   inherit inputs;
  #   repoRoot = ./.;
  #   systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" "aarch64-linux" ];
  #   flake = _:
  #     let
  #       inherit (nixpkgs) lib;
  #     in
  #     {
  #       sqitch-plan-dirs = {
  #         # Ensure this path only changes when sqitch.plan file is updated, or DDL
  #         # files are updated.
  #         chain-sync = (builtins.path {
  #           path = self;
  #           name = "marlowe-chain-sync-sqitch-plan";
  #           filter = path: type:
  #             path == "${self}/marlowe-chain-sync"
  #               || path == "${self}/marlowe-chain-sync/sqitch.plan"
  #               || lib.hasPrefix "${self}/marlowe-chain-sync/deploy" path
  #               || lib.hasPrefix "${self}/marlowe-chain-sync/revert" path;
  #         }) + "/marlowe-chain-sync";

  #         # Ensure this path only changes when sqitch.plan file is updated, or DDL
  #         # files are updated.
  #         runtime = (builtins.path {
  #           path = self;
  #           name = "marlowe-runtime-sqitch-plan";
  #           filter = path: type:
  #             path == "${self}/marlowe-runtime"
  #               || path == "${self}/marlowe-runtime/marlowe-indexer"
  #               || path == "${self}/marlowe-runtime/marlowe-indexer/sqitch.plan"
  #               || lib.hasPrefix "${self}/marlowe-runtime/marlowe-indexer/deploy" path
  #               || lib.hasPrefix "${self}/marlowe-runtime/marlowe-indexer/revert" path;
  #         }) + "/marlowe-runtime/marlowe-indexer";
  #       };

  #       nixosModules.default = import ./nix/nixos.nix inputs;
  #     };
  #   outputs = import ./nix/outputs.nix;
  # };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
