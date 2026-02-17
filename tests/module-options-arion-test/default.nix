{ pkgs, lib ? pkgs.lib, ... }:
let
  inherit (lib) any escapeShellArg replaceStrings runTests toList;

  evalComposition = modules: import ../../src/nix/eval-composition.nix {
    inherit pkgs;
    modules = toList modules;
  };

  testComposition = { expected, fn, config }: {
    inherit expected;
    expr = fn (evalComposition config);
  };

  search = pattern: str: (builtins.match ".*${pattern}.*" str) != null;

  assertionsMatch = patterns: assertions:
    let
      failed = builtins.filter (assertion: !assertion.assertion) assertions;
      matchAnyPattern = assertion: any (pattern: search pattern assertion.message) (toList patterns);
    in
    any matchAnyPattern failed;

  checkAssertions = expected: patterns: config: testComposition {
    inherit expected config;
    fn = composition: assertionsMatch patterns composition.config.assertions;
  };

  checkAssertionsMatch = checkAssertions true;
  checkAssertionsDoNotMatch = checkAssertions false;

  mkRepoTagAssertionPattern = component: attrName: name: replaceStrings [ "\n" ] [ " " ] ''
    Unable to infer the ${component} of the image associated with
    config\.services\.${name}\.  Please set
    config\.services\.${name}\.image\.${attrName} to a non-empty string\.
  '';

  imageNameAssertionPattern = mkRepoTagAssertionPattern "name" "imageName";
  imageTagAssertionPattern = mkRepoTagAssertionPattern "tag" "imageTag";

  tests = runTests {
    testNoImageName = checkAssertionsMatch (imageNameAssertionPattern "no-name") {
      services.no-name.image = {
        tarball = "/no/name.tar.gz";
        tag = "test";
      };
    };

    testNoImageTag = checkAssertionsMatch (imageTagAssertionPattern "no-tag") {
      services.no-tag.image = {
        tarball = "/no/tag.tar.gz";
        name = "test";
      };
    };

    testImageNameInference = testComposition {
      config = {
        services.nix-store-path.image.tarball = builtins.storeDir + "/foo-bar-baz.tar.gz";
      };
      expected = "bar-baz";
      fn = composition: composition.config.services.nix-store-path.image.tarball.imageName;
    };

    testImageTagInference = testComposition {
      config = {
        services.nix-store-path.image.tarball = builtins.storeDir + "/foo-bar-baz.tar.gz";
      };
      expected = "foo";
      fn = composition: composition.config.services.nix-store-path.image.tarball.imageTag;
    };
  };
in
# Abort if `tests`(list containing failed tests) is not empty
pkgs.runCommandNoCC "module-options-arion-test" { } ''
  ${pkgs.jq}/bin/jq 'if . == [] then . else halt_error(1) end' > "$out" <<<${escapeShellArg (builtins.toJSON tests)}
''
