{
  perSystem = { pkgs, final, ... }:
    let
      inherit (final) arion lib;
      inherit (final.testers) runNixOSTest;
    in
    {
      checks = lib.optionalAttrs pkgs.stdenv.isLinux {
        test = runNixOSTest {
          imports = [ ./arion-test ];
        };

        nixosModuleWithDocker =
          runNixOSTest {
            imports = [ ./nixos-virtualization-arion-test/test.nix ];
            extraBaseModules = {
              virtualisation.arion.backend = "docker";
            };
          };

        # Currently broken; kafka can't reach zookeeper
        # nixosModuleWithPodman =
        #   runNixOSTest {
        #     imports = [ ./nixos-virtualization-arion-test/test.nix ];
        #     extraBaseModules = {
        #       virtualisation.arion.backend = "podman-socket";
        #     };
        #   };

        testWithPodman =
          runNixOSTest {
            imports = [ ./arion-test ];
            _module.args.usePodman = true;
          };

        testBuild = arion.build {

          # To be more accurate, we could do
          #   pkgs = import ../examples/minimal/arion-pkgs.nix;
          # But let's avoid re-evaluating Nixpkgs
          pkgs = final;

          modules = [ ../examples/minimal/arion-compose.nix ];
        };

        testModuleOptions = import ./module-options-arion-test {
          inherit lib;
          pkgs = final;
        };
      };
    };
}
