{
  perSystem = { pkgs, final, ... }:
    let
      inherit (final) nixosTest arion lib;
    in
    {
      checks = lib.optionalAttrs pkgs.stdenv.isLinux {
        test = nixosTest ./arion-test;

        nixosModuleWithDocker =
          import ./nixos-virtualization-arion-test/test.nix final {
            virtualisation.arion.backend = "docker";
          };

        # Currently broken; kafka can't reach zookeeper
        # nixosModuleWithPodman =
        #   import ./nixos-virtualization-arion-test/test.nix final {
        #     virtualisation.arion.backend = "podman-socket";
        #   };

        testWithPodman =
          nixosTest (import ./arion-test { usePodman = true; pkgs = final; });

        testBuild = arion.build {

          # To be more accurate, we could do
          #   pkgs = import ../examples/minimal/arion-pkgs.nix;
          # But let's avoid re-evaluating Nixpkgs
          pkgs = final;

          modules = [ ../examples/minimal/arion-compose.nix ];
        };

      };
    };
}
