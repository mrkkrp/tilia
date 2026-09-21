{
  description = "A formatter for Haskell source code";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # Stackage is not used here; pointing it at an empty flake stops
    # nix-direnv from downloading the snapshot on every shell entry.
    haskellNix.inputs.stackage.follows = "emptyFlake";
    emptyFlake.url = "github:input-output-hk/empty-flake";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, haskellNix, flake-utils, ... }:
    let
      inherit (nixpkgs) lib;

      compilers = [ "ghc9103" "ghc9124" "ghc9141" ];
      baseCompiler = builtins.head compilers;

      # Files that participate in the build. Anything outside this set can
      # change without forcing a rebuild.
      sourceDirs = [ "src" "app" "tests" "corpora" ];
      sourceFiles = [ "cabal.project" "tilia.cabal" ];

      # Cabal insists these exist, but their contents never affect the
      # build, so they are staged as empty placeholders.
      placeholders = [ "LICENSE.md" "CHANGELOG.md" "README.md" ];
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay ];
        };

        src =
          let
            root = toString ./.;
            wanted = path:
              let rel = lib.removePrefix "${root}/" (toString path); in
              lib.elem rel sourceFiles
              || lib.any (d: rel == d || lib.hasPrefix "${d}/" rel) sourceDirs;
            pruned = pkgs.haskell-nix.haskellLib.cleanSourceWith {
              name = "tilia-source";
              src = ./.;
              filter = path: _type: wanted path;
            };
          in
          pkgs.runCommand "tilia-source-staged" { } ''
            cp -r ${pruned} $out
            chmod -R u+w $out
            touch ${lib.concatMapStringsSep " " (f: "$out/${f}") placeholders}
          '';

        projects = lib.genAttrs compilers (compiler:
          pkgs.haskell-nix.cabalProject {
            inherit src;
            compiler-nix-name = compiler;
            modules = [{ packages.tilia.writeHieFiles = true; }];
          });

        weeder =
          let
            project = projects.${baseCompiler};
            inherit (project.tilia.components) library exes tests;
            scanned = [ library exes.tilia tests.tests ];
          in
          pkgs.runCommand "tilia-weeder"
            { nativeBuildInputs = [ (project.tool "weeder" "2.10.0") ]; }
            ''
              weeder --config ${./weeder.toml} \
                ${lib.concatMapStringsSep " \\\n    "
                    (c: "--hie-directory ${c.hie}") scanned}
              touch $out
            '';

        perGHC = lib.genAttrs compilers (compiler:
          let
            inherit (projects.${compiler}) tilia;
            built = {
              tilia = tilia.components.exes.tilia;
              tests-exe = tilia.components.tests.tests;
            }
            // lib.optionalAttrs (compiler == baseCompiler) { inherit weeder; };
          in
          built // { ci = pkgs.linkFarm "tilia-ci-${compiler}" built; });

        base = perGHC.${baseCompiler};

        release = pkgs.haskell-nix.cabalProject {
          inherit src;
          compiler-nix-name = baseCompiler;
          modules = [{
            packages.tilia.components.exes.tilia = {
              configureFlags = [ "--ghc-option=-optl=-static" ];
              dontStrip = false;
            };
          }];
        };

        releaseBinary =
          release.projectCross.musl64.hsPkgs.tilia.components.exes.tilia;

        checking = name: tools: run:
          pkgs.runCommand "tilia-${name}" { nativeBuildInputs = tools; } ''
            ${run}
            touch $out
          '';

        nixSource = lib.cleanSourceWith {
          name = "tilia-nix-source";
          src = ./.;
          filter = path: type: type == "directory" || lib.hasSuffix ".nix" path;
        };

        tidy = {
          cabal-gild = checking "cabal-gild" [ pkgs.haskellPackages.cabal-gild ] ''
            cabal-gild --input=${./tilia.cabal} --mode=check
          '';
          nixpkgs-fmt = checking "nixpkgs-fmt" [ pkgs.nixpkgs-fmt ] ''
            nixpkgs-fmt --check ${nixSource}
          '';
          deadnix = checking "deadnix" [ pkgs.deadnix ] ''
            deadnix --fail ${nixSource}
          '';
        };

        shellFor = compiler: projects.${compiler}.shellFor {
          tools.cabal = "latest";
          nativeBuildInputs = [
            pkgs.haskellPackages.cabal-gild
            pkgs.nixpkgs-fmt
            pkgs.deadnix
          ];
          withHoogle = false;
          exactDeps = false;
        };

        format = pkgs.writeShellApplication {
          name = "tilia-format";
          runtimeInputs = [
            base.tilia
            pkgs.cabal-install
            pkgs.haskell-nix.compiler.${baseCompiler}
            pkgs.haskellPackages.cabal-gild
            pkgs.nixpkgs-fmt
          ];
          text = ''
            export LANG=C.UTF-8
            tilia inplace all --check-ast --check-idempotence
            cabal-gild --io=tilia.cabal --mode=format
            nixpkgs-fmt ./*.nix
          '';
        };
      in
      {
        packages = {
          default = base.tilia;
          lint = pkgs.linkFarm "tilia-lint" tidy;
          release = releaseBinary;
        };

        checks = { inherit weeder; } // tidy;

        apps = {
          default = {
            type = "app";
            program = "${base.tilia}/bin/tilia";
          };
          format = {
            type = "app";
            program = "${format}/bin/tilia-format";
          };
        };

        devShells = { default = shellFor baseCompiler; }
          // lib.genAttrs compilers shellFor;

        legacyPackages = base // perGHC;
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://tilia.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "tilia.cachix.org-1:bxzzQCOu9D/Suuzll8oRj2RaOb37KVTsETMiTDMLiJ4="
    ];
  };
}
