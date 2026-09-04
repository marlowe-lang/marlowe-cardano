{ inputs, pkgs, lib, project, ghc }:

let
  tools = {
    cabal = project.tool "cabal" "3.12.1.0";
    cabal-fmt = project.tool "cabal-fmt" "latest";
    fourmolu = project.tool "fourmolu" "latest";
    haskell-language-server = project.tool "haskell-language-server" "2.12.0.0";
    hlint = project.tool "hlint" "latest";
    implicit-hie = project.tool "implicit-hie" "latest";
    stylish-haskell = project.tool "stylish-haskell" "latest";
  };

  db-schema-info = pkgs.callPackage ./shell/db-schema-info.nix {};

  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {
    src = lib.cleanSources ../.;

    hooks = {
      nixpkgs-fmt = {
        enable = false;
        package = pkgs.nixpkgs-fmt;
      };
      cabal-fmt = {
        enable = false;
        package = tools.cabal-fmt;
      };
      stylish-haskell = {
        enable = false;
        package = tools.stylish-haskell;
        args = [ "--config" ".stylish-haskell.yaml" ];
      };
      fourmolu = {
        enable = false;
        package = tools.fourmolu;
      };
      hlint = {
        enable = false;
        package = tools.hlint;
        args = [ "--hint" ".hlint.yaml" ];
      };
      shellcheck = {
        enable = false;
        package = pkgs.shellcheck;
      };
    };
  };

  cardano-cli = inputs.cardano-node.packages.${pkgs.system}.cardano-cli;
  cardano-node = inputs.cardano-node.packages.${pkgs.system}.cardano-node;

  commonPackages = [
    cardano-cli
    cardonnay

    tools.cabal
    tools.cabal-fmt
    tools.fourmolu
    tools.haskell-language-server
    tools.haskell-language-server.package.components.exes.haskell-language-server-wrapper
    tools.hlint
    tools.implicit-hie
    tools.stylish-haskell

    # Inspect capkgs:
    # (builtins.trace (lib.concatStringsSep ", " (lib.attrNames inputs.capkgs.packages.${pkgs.system})) inputs.capkgs)
    inputs.capkgs.packages.${pkgs.system}.bech32-input-output-hk-cardano-node-10-7-1-045bc18
    inputs.capkgs.packages.${pkgs.system}."\"cardano-addresses:exe:cardano-address\"-IntersectMBO-cardano-addresses-4-0-2-5c00d7b"

    pkgs.act
    pkgs.bash
    pkgs.bzip2
    pkgs.b2sum
    pkgs.cacert
    pkgs.coreutils
    pkgs.curl
    pkgs.fd
    pkgs.gawk
    pkgs.git
    pkgs.gnused
    pkgs.jq
    pkgs.nixpkgs-fmt
    pkgs.nodejs
    pkgs.perl
    pkgs.postgresql
    pkgs.postgresql.lib
    pkgs.postgresql.dev
    pkgs.python3
    pkgs.ripgrep
    pkgs.sqitchPg
    pkgs.shellcheck
    pkgs.which
    pkgs.yarn-berry
    pkgs.yarn-berry_4.yarn-berry-fetcher
    pkgs.yarn-bash-completion
    pkgs.z3
    pkgs.zlib
  ];

  commonShellHook = ''
    ${preCommitCheck.shellHook}
    export ROOT_DIR="$(git rev-parse --show-toplevel)"
    export RUN_DIR="$ROOT_DIR/.run"

    # Vars required by postgres part of the process compose:
    export SQITCH_CHDIR="$ROOT_DIR/sql"
    export POSTGRES_DIR="$RUN_DIR/postgres"
    export PGPORT="15432"

    # Vars required by testnet part of the process compose:
    export TESTNET_DIR="$RUN_DIR/testnet"
    export CARDONNAY_TESTNET_ID="9"
    export CARDANO_NODE_NETWORK_ID=42
    source <(cardonnay control print-env -i "$CARDONNAY_TESTNET_ID" -w "$TESTNET_DIR")

    # This **will be** initialized by the testnet process compose when executed
    export FAUCET_ADDR_FILE="$TESTNET_DIR/faucet.addr"
    export FAUCET_SKEY_FILE="$TESTNET_DIR/faucet.skey"
    export MARLOWE_PUBLISHING_INFO_FILE=$"$TESTNET_DIR/marlowe-publishing-info.json"

    export PROCESS_COMPOSE_TESTNET_YAML=${process-compose-testnet-yaml}
    export PROCESS_COMPOSE_POSTGRES_YAML=${process-compose-postgres-yaml}
    export PROCESS_COMPOSE_DEV_ENV_YAML=${process-compose-dev-env-yaml}

    export LD_LIBRARY_PATH="${ld-library-path}:$LD_LIBRARY_PATH"
    export PATH=$PATH:"$ROOT_DIR/marlowe-integration/node_modules/.bin"
  '';

  systemLevelLibDeps = project.shellFor {
    packages = p: [p.cardano-crypto-class p.ouroboros-consensus];
    withHoogle = false;
  };

  commonJail = {
    # jail.combinators.unshare-all
    #     jail.combinators.mount-cwd
    #     (jail.combinators.try-fwd-env "PKG_CONFIG_PATH")
    #   ];
    baseJailOptions =
      let
        jail = inputs.jailed-agents.lib.${pkgs.system}.internals.jail;
      in [
        jail.combinators.network
        jail.combinators.time-zone
        jail.combinators.no-new-session
        jail.combinators.mount-cwd
        (jail.combinators.try-fwd-env "PKG_CONFIG_PATH")
        (jail.combinators.try-fwd-env "LD_LIBRARY_PATH")

        (jail.combinators.try-fwd-env "ROOT_DIR")
        (jail.combinators.try-fwd-env "RUN_DIR")
        (jail.combinators.try-fwd-env "SQITCH_CHDIR")
        (jail.combinators.try-fwd-env "POSTGRES_DIR")
        (jail.combinators.try-fwd-env "PGPORT")
        (jail.combinators.try-fwd-env "TESTNET_DIR")
        (jail.combinators.try-fwd-env "CARDONNAY_TESTNET_ID")
        (jail.combinators.try-fwd-env "CARDANO_NODE_NETWORK_ID")
        (jail.combinators.try-fwd-env "CARDANO_NODE_SOCKET_PATH")
        (jail.combinators.try-fwd-env "FAUCET_ADDR_FILE")
        (jail.combinators.try-fwd-env "FAUCET_SKEY_FILE")
        (jail.combinators.try-fwd-env "MARLOWE_PUBLISHING_INFO_FILE")
        # we pass only the information about the running dev env
        (jail.combinators.try-fwd-env "PROCESS_COMPOSE_DEV_ENV_YAML")
      ];

    extraReadwriteDirs = [
      "/home/paluh/.config/cabal"   # exactly the path it complains about
      "/home/paluh/.cache/cabal"          # also include the classic cabal dir (safe)
      "/home/paluh/.cabal-devx"          # also include the classic cabal dir (safe)
      "/home/paluh/.local/state/cabal"          # also include the classic cabal dir (safe)
      "/home/paluh/.local/bin/cabal-plan"          # also include the classic cabal dir (safe)
      "/home/paluh/.local/bin/ghcid"          # also include the classic cabal dir (safe)
      "/home/paluh/.config/opencode"
      "/home/paluh/.local/share/opencode"
      "/home/paluh/.local/state/opencode"
      "/home/paluh/.cache/opencode"
      "/home/paluh/projects/cl/"
      "/home/paluh/programming/cardano/mgdoc"
      "/tmp"
    ];
    # (builtins.trace (lib.concatStringsSep ", " (lib.attrNames project.hsPkgs.cardano-crypto-class.components.library)) project)
    # (builtins.trace (lib.concatStringsSep ", " cryptoShell.nativeBuildInputs) cryptoShell)
    # pkgs.haskell-nix.compiler.${ghc}
    extraPkgs = systemLevelLibDeps.nativeBuildInputs ++ systemLevelLibDeps.buildInputs ++ commonPackages ++ [
      pkgs.stdenv.cc.cc.lib
    ];
  };

  cardonnay = pkgs.python313.pkgs.buildPythonApplication {
    pname = "cardonnay";
    version = "0.3.4";
    SETUPTOOLS_SCM_PRETEND_VERSION = "0.3.4";
    src = inputs.cardonnay-src;
    pyproject = true;
    build-system = with pkgs.python313.pkgs; [ setuptools setuptools-scm ];
    pythonRelaxDeps = [ "setuptools" ];
    nativeBuildInputs = with pkgs.python313.pkgs; [
      pythonRelaxDepsHook
    ];
    postPatch = ''
      # Reduce initial TX submission delay (safe for local testnets)
      # find src/cardonnay_scripts/scripts \
      #   -name 'common-start-*' -type f -exec \
      #   sed -i 's/readonly TX_SUBMISSION_DELAY=60/readonly TX_SUBMISSION_DELAY=20/' {} +
    '';
    dependencies = with pkgs.python313.pkgs; [
      supervisor
      click
      pygments
      pydantic
      filelock
    ];
  };

  process-compose-postgres-yaml = pkgs.callPackage ./process-compose/postgres.nix {};

  process-compose-postgres = pkgs.writeShellApplication {
    name = "process-compose-postgres";
    runtimeInputs = [];
    text = ''
      ${pkgs.process-compose}/bin/process-compose up -f ${process-compose-postgres-yaml} -L "$RUN_DIR"/process-compose-postgres;
    '';
  };

  process-compose-testnet-yaml = pkgs.callPackage ./process-compose/testnet.nix {
    inherit cardonnay cardano-node cardano-cli;
  };

  process-compose-testnet = pkgs.writeShellApplication {
    name = "process-compose-testnet";
    runtimeInputs = [];
    text = ''
      ${pkgs.process-compose}/bin/process-compose up -f ${process-compose-testnet-yaml} -L "$RUN_DIR"/process-compose-testnet;
    '';
  };

  process-compose-dev-env-yaml = pkgs.callPackage ./process-compose/dev-env.nix {
    inherit cardano-cli cardano-node cardonnay;
  };

  process-compose-dev-env = pkgs.writeShellApplication {
    name = "process-compose-dev-env";
    runtimeInputs = [];
    text = ''
      ${pkgs.process-compose}/bin/process-compose up -f ${process-compose-dev-env-yaml} -L "$RUN_DIR"/process-compose-dev-env;
    '';
  };

  ld-library-path = lib.makeLibraryPath [
    pkgs.stdenv.cc.cc.lib
    pkgs.bzip2
    # add more only if you hit further .so errors (e.g. pkgs.zlib)
  ];

  shell = project.shellFor {
    name = "marlowe-plutus-${project.args.compiler-nix-name}";

    nativeBuildInputs = commonPackages ++ [
      cardano-node
      # inputs.process-compose
      pkgs.process-compose
      pkgs.dbeaver-bin
      pkgs.mitmproxy

      # db-schema-info generator

      # The main process compose for the full dev env.
      process-compose-dev-env
      # FIXME: Currently those process compose envs will overlap
      # regarding port. They are exposed for debugging purposes.
      process-compose-postgres
      process-compose-testnet

      (inputs.jailed-agents.lib.${pkgs.system}.makeJailedOpencode {
        inherit (commonJail) baseJailOptions extraPkgs extraReadwriteDirs;
        env = {
          LD_LIBRARY_PATH = ld-library-path;
        };
      })
      (inputs.jailed-agents.lib.${pkgs.system}.makeJailedOpencode {
        name = "jailed-bash";
        pkg = pkgs.bashInteractive;
        # configPaths = [
        #   "~/.bashrc"
        #   "~/.inputrc"
        # ];
        inherit (commonJail) baseJailOptions extraPkgs extraReadwriteDirs;
      })
    ];

    # To make shell lightweight compile only the packages
    # which pull in external dependencies like
    # cardano-crypto-class which brings in libsodium, secp256k1, etc.
    packages = p: [p.cardano-crypto-class p.ouroboros-consensus];

    withHoogle = false;

    shellHook = ''
      ${preCommitCheck.shellHook}
      export ROOT_DIR="$(git rev-parse --show-toplevel)"
      export RUN_DIR="$ROOT_DIR/.run"

      # Vars required by postgres part of the process compose:
      export SQITCH_CHDIR="$ROOT_DIR/sql"
      export POSTGRES_DIR="$RUN_DIR/postgres"
      export PGPORT="15432"

      # Vars required by testnet part of the process compose:
      export TESTNET_DIR="$RUN_DIR/testnet"
      export CARDONNAY_TESTNET_ID="9"
      export CARDANO_NODE_NETWORK_ID=42
      source <(cardonnay control print-env -i "$CARDONNAY_TESTNET_ID" -w "$TESTNET_DIR")

      # This **will be** initialized by the testnet process compose when executed
      export FAUCET_ADDR_FILE="$TESTNET_DIR/faucet.addr"
      export FAUCET_SKEY_FILE="$TESTNET_DIR/faucet.skey"
      export MARLOWE_PUBLISHING_INFO_FILE=$"$TESTNET_DIR/marlowe-publishing-info.json"

      export PROCESS_COMPOSE_TESTNET_YAML=${process-compose-testnet-yaml}
      export PROCESS_COMPOSE_POSTGRES_YAML=${process-compose-postgres-yaml}
      export PROCESS_COMPOSE_DEV_ENV_YAML=${process-compose-dev-env-yaml}
      export PATH=$PATH:"$ROOT_DIR/marlowe-integration/node_modules/.bin"

      export LD_LIBRARY_PATH="${ld-library-path}:$LD_LIBRARY_PATH"

      # ${db-schema-info}/bin/db-schema-info sql/final
    '';
  };
in
  shell
