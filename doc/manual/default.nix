{ pkgs ? import ../../nix {}, version ? "local" }:
let
  inherit (pkgs) recurseIntoAttrs callPackage runCommand lib stdenv ;

  nixosManualPath = s: "${pkgs.path}/nixos/doc/manual/${s}";

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
        --stringparam revision '${version}' \
        --stringparam sourceUrl 'https://github.com/hercules-ci/arion/blob/${version}' \
        -o intermediate.xml ${./options-to-docbook.xsl} $optionsXML
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

  fixPaths = opt: opt // builtins.trace opt.declarations {
    declarations = map (d: lib.strings.removePrefix "/" (lib.strings.removePrefix (toString ../..) (toString d))) opt.declarations;
  };

in

recurseIntoAttrs rec {
  compositionOptions = options {
    moduleType = "composition";
    description = "List of Arion composition-level options in JSON format";
    optionsList = let composition = import ../../src/nix/eval-composition.nix { inherit pkgs; };
                  in map fixPaths (lib.filter (opt: opt.visible && !opt.internal) (lib.optionAttrSetToDocList composition.options));
  };
  serviceOptions = options {
    moduleType = "service";
    description = "List of Arion service-level options in JSON format";
    optionsList = let service = pkgs.callPackage ../../src/nix/eval-service.nix {} { modules = []; uid = -1; };
                  in map fixPaths (lib.filter (opt: opt.visible && !opt.internal) (lib.optionAttrSetToDocList service.options));
  };
  generatedDocBook = runCommand "generated-docbook" {} ''
    mkdir $out
    ln -s ${compositionOptions.optionsDocBook} $out/options-composition.xml
    ln -s ${serviceOptions.optionsDocBook} $out/options-service.xml
  '';
  manual = stdenv.mkDerivation {
    src = lib.sourceByRegex ./. [
      "Makefile$"
      ".*\.asciidoc$"
      ".*\.xsl$"
      ".*\.css$"
      "^manual.xml$"
      "^manual.xml$"
    ];
    name = "arion-manual";
    version = version;
    buildInputs = [
      (pkgs.libxslt.bin or pkgs.libxslt)
      pkgs.asciidoctor
    ];
    XML_CATALOG_FILES = "${pkgs.docbook_xsl}/xml/xsl/docbook/catalog.xml";
    inherit generatedDocBook;
    configurePhase = ''
      export docdir=$out/doc
    '';
    postPatch = ''
      substituteInPlace manual.xml --subst-var version
    '';
    prePatch = ''
      set -x
      cp ${generatedDocBook}/* .
      substituteInPlace options-service.xml \
        --replace 'xml:id="appendix-configuration-options"' 'xml:id="appendix-service-options"' \
        --replace '<title>Configuration Options</title>' '<title>Service Options</title>' \
        --replace 'xml:id="configuration-variable-list"' 'xml:id="service-variable-list"' \
        ;
      substituteInPlace options-composition.xml \
        --replace 'xml:id="appendix-configuration-options"' 'xml:id="appendix-composition-options"' \
        --replace '<title>Configuration Options</title>' '<title>Composition Options</title>' \
        --replace 'xml:id="configuration-variable-list"' 'xml:id="composition-variable-list"' \
        ;
      ls -R
      set +x
    '';
    shellHook = ''
      live-build() {
        inotifywait -e MODIFY -m -r . | while read; do
          make
        done
      }
    '';
  };
}
