{
  perSystem = { pkgs, final, ... }:
    let
      inherit (final) arion lib;
      inherit (final.testers) runNixOSTest;

      serviceAEnvironment = module:
        (arion.eval {
          pkgs = final;
          modules = [ module ];
        }).config.out.dockerComposeYamlAttrs.services.a.environment;

      checkEqual = name: actual: expected:
        if actual == expected
        then pkgs.emptyFile
        else throw "${name}: expected ${builtins.toJSON expected}, got ${builtins.toJSON actual}";

      extend = import ./service-submodule/extend.nix;
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

        testServiceImports = checkEqual "testServiceImports"
          (serviceAEnvironment ./service-submodule/imports.nix)
          { IMPORTED = "yes"; };

        testServiceExtendSubmoduleWith = checkEqual "testServiceExtendSubmoduleWith"
          (serviceAEnvironment (extend (lib: module:
            lib.types.submoduleWith { modules = [ module ]; })))
          { GREETING = "hello"; };

        # null takes arion's own shorthandOnlyDefinesConfig, so this declaration
        # also merges with an arion that sets it to true.
        testServiceExtendSubmoduleWithNull = checkEqual "testServiceExtendSubmoduleWithNull"
          (serviceAEnvironment (extend (lib: module:
            lib.types.submoduleWith {
              modules = [ module ];
              shorthandOnlyDefinesConfig = null;
            })))
          { GREETING = "hello"; };

        # Redeclaring services with types.submodule conflicts on
        # shorthandOnlyDefinesConfig.
        testServiceExtendSubmoduleFails = checkEqual "testServiceExtendSubmoduleFails"
          (builtins.tryEval (builtins.deepSeq
            (serviceAEnvironment (extend (lib: module: lib.types.submodule module)))
            true))
          { success = false; value = false; };

      };
    };
}
