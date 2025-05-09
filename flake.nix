{
  description = "Arion - use Docker Compose via Nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskell-flake.url = "github:srid/haskell-flake/0.1.0";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ config, lib, extendModules, ... }: {
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-parts.flakeModules.easyOverlay
        inputs.flake-parts.flakeModules.partitions
        ./docs/flake-module.nix
      ];

      partitions.dev = {
        extraInputsFlake = ./dev;
        module = { inputs, ... }: {
          imports = [
            inputs.hercules-ci-effects.flakeModule
            inputs.git-hooks-nix.flakeModule
            ./tests/flake-module.nix
            ./dev/flake-module.nix
          ];
        };
      };
      partitionedAttrs.devShells = "dev";
      partitionedAttrs.checks = "dev";
      partitionedAttrs.herculesCI = "dev";

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
        };

      flake = {
        debug = { inherit inputs config lib; };

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
