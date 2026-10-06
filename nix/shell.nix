{ repoRoot, inputs, pkgs, system, lib, project, ghcVersion ? "ghc984" }:
let
  tools = {
    cabal = (project.tool "cabal" "3.12.1.0");
    cabal-fmt = (project.tool "cabal-fmt" "latest");
    cabal-plan = (project.tool "cabal-plan" "latest");
    fourmolu = (project.tool "fourmolu" "latest");
    haskell-language-server = (project.tool "haskell-language-server" "2.12.0.0");
    hlint = (project.tool "hlint" "latest");
    stylish-haskell = (project.tool "stylish-haskell" "latest");
  };

  # This is an alternative way to get cabal-plan if needed:
  # cabal-plan = (pkgs.haskell-nix.hackage-package {
  #   compiler-nix-name = ghcVersion;
  #   name = "cabal-plan";
  #   version = "latest";
  # }).components.exes.cabal-plan;

  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {
    src = lib.cleanSources ../.;
    hooks = {
      cabal-fmt = {
        enable = true;
        package = tools.cabal-fmt;
        args = [ "--no-tabular" ];
      };
      fourmolu = {
        enable = true;
        package = tools.fourmolu;
      };
      hlint = {
        enable = true;
        package = tools.hlint;
        # args = [ "--hint" ".hlint.yaml" ];
      };
      nixpkgs-fmt = {
        enable = true;
        package = pkgs.nixpkgs-fmt;
      };
      shellcheck = {
        enable = true;
        package = pkgs.shellcheck;
      };
      stylish-haskell = {
        enable = false;
        package = tools.stylish-haskell;
        args = [ "--config" ".stylish-haskell.yaml" ];
      };
    };
  };

  shell = project.shellFor {
    buildInputs = [
      pkgs.z3
      pkgs.sqitchPg
      pkgs.postgresql
      # pkgs.scriv
      pkgs.jq

      tools.cabal
      tools.cabal-fmt
      tools.cabal-plan
      tools.fourmolu
      tools.haskell-language-server
      tools.haskell-language-server.package.components.exes.haskell-language-server-wrapper
      tools.hlint
      tools.stylish-haskell
    ];

    nativeBuildInputs = [
      pkgs.libsodium-vrf
      pkgs.libblst
      pkgs.lmdb
      pkgs.secp256k1
      pkgs.libpq
      pkgs.bzip2
      pkgs.pkg-config
    ];

    name = "marlowe-cardano";

    # If you want to prevent the `nix develop` from building all
    # Haskell packages in the project, you can set this to an
    # empty list.
    # If you want to build them all just comment `packages` out.
    packages = projectPkgs: [ ];

    withHoogle = false;

    shellHook = ''
      ${preCommitCheck.shellHook}
    '';
  };
in
shell

# { repoRoot, inputs, pkgs, lib, system, project }:
#
# let
#
#   scripts = repoRoot.nix.marlowe-cardano.scripts;
#
#   isLinux = pkgs.stdenv.hostPlatform.isLinux;
#
# in
#
# {
#   name = "marlowe-cardano";
#
#
#   packages = [
#     cabalProject.hsPkgs.hspec-golden.components.exes.hgold
#
#     repoRoot.nix.marlowe-cardano.cardano-tools.cardano-node
#     repoRoot.nix.marlowe-cardano.cardano-tools.cardano-cli
#     repoRoot.nix.marlowe-cardano.cardano-tools.cardano-addresses
#
#     inputs.marlowe-plutus.packages.marlowe-minting-validator
#     inputs.n2c.packages.skopeo-nix2container
#
#     pkgs.z3
#     pkgs.sqitchPg
#     pkgs.postgresql
#     pkgs.scriv
#     pkgs.jq
#     pkgs.docker-compose
#   ];
#
#
#   env.PGUSER = "postgres";
#
#
#   scripts = {
#
#     re-up = {
#       description = "Builds compose.nix, (re)creates and (re)starts the dev docker containers for Runtime.";
#       exec = scripts.re-up;
#       enable = isLinux;
#       group = "marlowe";
#     };
#
#     refresh-compose = {
#       description = "Genereate compose.yaml in the repository root";
#       exec = scripts.refresh-compose;
#       enable = isLinux;
#       group = "marlowe";
#     };
#
#     marlowe-cli = {
#       exec = scripts.marlowe-cli;
#       description = "Marlowe CLI";
#       group = "marlowe";
#     };
#
#     refresh-validators = {
#       exec = scripts.refresh-validators;
#       description = "Pull the latest validators from the marlowe-plutus flake input.";
#       group = "marlowe";
#     };
#   };
#
#
#   shellHook = lib.optionalString isLinux "refresh-compose";
#
#   preCommit = {
#     cabal-fmt.enable = true;
#     cabal-fmt.extraOptions = "--no-tabular";
#     nixpkgs-fmt.enable = true;
#     shellcheck.enable = true;
#     fourmolu.enable = true;
#     hlint.enable = true;
#   };
# }
