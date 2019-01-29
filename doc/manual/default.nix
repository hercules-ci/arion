{ pkgs ? import ../../nix {} }:
let
  inherit (pkgs) recurseIntoAttrs callPackage runCommand lib;

  nixosManualPath = s: "${pkgs.path}/nixos/doc/manual/${s}";
  revision = "0.0-fixme"; # FIXME

  # NixOS module system options in JSON format.
  options = { moduleType, description, optionsList }: recurseIntoAttrs rec {
    optionsXML = builtins.toFile "options.xml" (builtins.toXML optionsList);

    optionsDocBook = runCommand "options-db.xml" {} ''
      optionsXML=${optionsXML}
      if grep /nixpkgs/nixos/modules $optionsXML; then
        echo "The manual appears to depend on the location of Nixpkgs, which is bad"
        echo "since this prevents sharing via the NixOS channel.  This is typically"
        echo "caused by an option default that refers to a relative path (see above"
        echo "for hints about the offending path)."
        exit 1
      fi
      ${pkgs.buildPackages.libxslt.bin}/bin/xsltproc \
        --stringparam revision '${revision}' \
        -o intermediate.xml ${nixosManualPath "options-to-docbook.xsl"} $optionsXML
      ${pkgs.buildPackages.libxslt.bin}/bin/xsltproc \
        -o "$out" ${nixosManualPath "postprocess-option-descriptions.xsl"} intermediate.xml
    '';
    
    optionsJSON = runCommand "${moduleType}-options-json" {
      meta.description = description;
    } ''
      # Export list of options in different format.
      dst=$out/share/doc/arion
      mkdir -p $dst

      cp ${builtins.toFile "options-${moduleType}.json" (builtins.unsafeDiscardStringContext (builtins.toJSON
        (builtins.listToAttrs (map (o: { name = o.name; value = removeAttrs o ["name" "visible" "internal"]; }) optionsList))))
      } $dst/options-${moduleType}.json

      mkdir -p $out/nix-support
      echo "file json $dst/options-${moduleType}.json" >> $out/nix-support/hydra-build-products
    '';

    
  };

in

recurseIntoAttrs rec {
  compositionOptions = options {
    moduleType = "composition";
    description = "List of Arion composition-level options in JSON format";
    optionsList = let composition = import ../../src/nix/eval-composition.nix { inherit pkgs; };
                  in lib.optionAttrSetToDocList composition.options;
  };
  serviceOptions = options {
    moduleType = "service";
    description = "List of Arion service-level options in JSON format";
    optionsList = let composition = pkgs.callPackage ../../src/nix/eval-service.nix {} { modules = []; uid = -1; };
                  in lib.optionAttrSetToDocList composition.options;
  };
  generatedDocBook = runCommand "generated-docbook" {} ''
    mkdir $out
    ln -s ${compositionOptions.optionsDocBook} $out/options-composition.xml
    ln -s ${serviceOptions.optionsDocBook} $out/options-service.xml
  '';
}
