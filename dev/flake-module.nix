{
  perSystem = { config, self', inputs', pkgs, system, final, ... }: {
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

}
