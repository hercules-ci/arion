{
  description = "Arion - use Docker Compose via Nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskell-flake.url = "github:srid/haskell-flake/0.1.0";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
    hercules-ci-effects.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ config, lib, extendModules, ... }: {
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.hercules-ci-effects.flakeModule
        inputs.flake-parts.flakeModules.easyOverlay
        ./docs/flake-module.nix
        ./tests/flake-module.nix
      ];
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      perSystem = { config, self', inputs', pkgs, system, final, ... }:
        let h = pkgs.haskell.lib.compose; in
        {
          overlayAttrs = {
            inherit (config.packages) arion;
            arionTestingFlags = {
              dockerSupportsSystemd = false;
            };
          };
          packages.default = config.packages.arion;
          packages.overlay-test = final.arion;
          packages.arion = import ./nix/arion.nix { inherit pkgs; };
          haskellProjects.haskell-package = {
            # not autodetected: https://github.com/srid/haskell-flake/issues/49
            packages.arion-compose.root = ./.;

            overrides =
              self: super: {
                arion-compose =
                  lib.pipe super.arion-compose [
                    (h.addBuildTools [ pkgs.nix ])
                    (h.overrideCabal (o: {
                      src = pkgs.lib.sourceByRegex ./. [
                        ".*[.]cabal"
                        "LICENSE"
                        "src/?.*"
                        "README.asciidoc"
                        "CHANGELOG.md"
                      ];
                      preCheck = ''
                        export NIX_LOG_DIR=$TMPDIR
                        export NIX_STATE_DIR=$TMPDIR
                        export NIX_PATH=nixpkgs=${pkgs.path}
                      '';
                    }))
                  ];
              };
          };
          devShells.default = config.devShells.haskell-package.overrideAttrs (o: {
            nativeBuildInputs = o.nativeBuildInputs or [ ] ++ [
              pkgs.docker-compose
              pkgs.nixpkgs-fmt
              config.haskellProjects.haskell-package.haskellPackages.releaser
            ];
          });
        };

      hercules-ci.flake-update = {
        enable = true;
        autoMergeMethod = "merge";
        when = {
          hour = [ 2 ];
          dayOfMonth = [ 5 ];
        };
      };

      herculesCI.ciSystems = [
        # "aarch64-darwin"
        # "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      flake = {
        debug = { inherit inputs config lib; };

        defaultPackage =
          lib.mapAttrs
            (ps: lib.warn "arion.defaultPackage has been removed in favor of arion.packages.\${system}.default"
              ps.default)
            config.flake.packages;

        lib = {
          eval = import ./src/nix/eval-composition.nix;
          build = args@{ ... }:
            let composition = self.lib.eval args;
            in composition.config.out.dockerComposeYaml;
        };
        nixosModules.arion = ./nixos-module.nix;
      };
    });
}
