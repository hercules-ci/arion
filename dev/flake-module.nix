{
  perSystem = { config, self', inputs', pkgs, system, final, ... }: {
    pre-commit.settings.hooks.ormolu.enable = true;
    devShells.default = config.devShells.haskell-package.overrideAttrs (o: {
      nativeBuildInputs = o.nativeBuildInputs or [ ] ++ [
        pkgs.docker-compose
        pkgs.nixpkgs-fmt
        config.haskellProjects.haskell-package.haskellPackages.releaser
      ];
      shellHook = ''
        ${config.pre-commit.installationScript}
        echo 1>&2 "Welcome to the arion dev shell"
      '';
    });
  };

  hercules-ci.flake-update = {
    enable = true;
    autoMergeMethod = "merge";
    baseMerge.enable = true;
    baseMerge.method = "merge";
    flakes = {
      "." = { };
      "dev" = { };
    };
    when = {
      hour = [ 2 ];
      dayOfMonth = [ 5 ];
    };
  };

  herculesCI.ciSystems = [
    "aarch64-darwin"
    # "aarch64-linux"
    # "x86_64-darwin"
    "x86_64-linux"
  ];

}
