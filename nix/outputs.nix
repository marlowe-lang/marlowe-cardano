{ repoRoot, inputs, pkgs, lib, system }:

let
  project = repoRoot.nix.project;

  staticPkgs = project.cabalProject.projectCross.musl64.hsPkgs;

  static =
    staticPkgs.marlowe-apps.components.exes //
    staticPkgs.marlowe-cli.components.exes;

  allStatic = pkgs.runCommand "all-statics" { } ''
    mkdir -p $out
    ${lib.concatMapStringsSep "\n" (drv: "cp ${drv}/bin/* $out") (lib.attrValues static)}
  '';
in
[
  # Default packages, apps, checks, devShells, hydraJobs for the Haskell project.
  (
    project.flake
  )

  {
    devShells.profiled = project.variants.profiled.devShell;

    inherit static allStatic;

    operables = repoRoot.nix.marlowe-cardano.deploy.operables;
    oci-images = repoRoot.nix.marlowe-cardano.deploy.oci-images;
    nomadTasks = repoRoot.nix.marlowe-cardano.deploy.nomadTasks;

    networks = repoRoot.nix.marlowe-cardano.networks;

    packages.integration-tests = repoRoot.nix.marlowe-cardano.integration-tests;

    checks.check-validators = repoRoot.nix.marlowe-cardano.check-validators;
  }

  (lib.optionalAttrs (system != "aarch64-darwin") {
    hydraJobs.operables = repoRoot.nix.marlowe-cardano.deploy.operables;
    hydraJobs.oci-images = repoRoot.nix.marlowe-cardano.deploy.oci-images;
  })

  (lib.optionalAttrs pkgs.stdenv.isLinux {
    hydraJobs.static = static;
    hydraJobs.allStatic = allStatic;
  })
]
