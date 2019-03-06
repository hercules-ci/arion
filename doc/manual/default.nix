{ pkgs ? import ../../nix {}
, version ? "none"
# Default sourceUrl is for local development. Works with
#   nix-build -A doc.manual
# For release, use: https://github.com/hercules-ci/arion/blob/${version}
, sourceUrl ? "../../.."
}:
let
  inherit (pkgs) recurseIntoAttrs callPackage runCommand lib stdenv ;

  nixosManualPath = s: "${pkgs.path}/nixos/doc/manual/${s}";

  # NixOS module system options in JSON format.
  options = { moduleType, description, /*optionsList*/ optionsExpr }: recurseIntoAttrs rec {
    optionsXML =
      # builtins.toFile "options.xml" (builtins.toXML optionsList);
      pkgs.runCommand "options.xml" {
        buildInputs = [pkgs.nix pkgs.jq];
        inherit optionsExpr;
      } ''
        export NIX_LOG_DIR=$PWD
        export NIX_STATE_DIR=$PWD
        nix-instantiate \
          --option sandbox false \
          --readonly-mode \
          --eval \
          --expr "$optionsExpr" \
          --xml \
          --strict \
          --show-trace \
          >$out
      '';

    optionsDocBook = runCommand "options-db.xml" {} ''
      optionsXML=${optionsXML}
      ${pkgs.buildPackages.libxslt.bin}/bin/xsltproc \
        --stringparam revision '${version}' \
        --stringparam sourceUrl '${sourceUrl}' \
        -o intermediate.xml ${./options-to-docbook.xsl} $optionsXML
      ${pkgs.buildPackages.libxslt.bin}/bin/xsltproc \
        -o "$out" ${nixosManualPath "postprocess-option-descriptions.xsl"} intermediate.xml
    '';
  };

  compositionOptions = options {
    moduleType = "composition";
    description = "List of Arion composition-level options in JSON format";
    optionsExpr = let
      src = ../../src/nix;
    in ''
      let pkgs = import ${pkgs.path} {};
          fixPaths = opt: opt // {
            declarations = map (d: "src/nix" + (lib.strings.removePrefix (toString ${src}) (toString d))) opt.declarations;
          };
          inherit (pkgs) lib;
          composition = import ${src}/eval-composition.nix { inherit pkgs; };
      in map fixPaths (lib.filter (opt: opt.visible && !opt.internal) (lib.optionAttrSetToDocList composition.options))
    '';
  };

  serviceOptions = options {
    moduleType = "service";
    description = "List of Arion service-level options in JSON format";
    optionsExpr = let
      src = ../../src/nix;
    in ''
      let pkgs = import ${pkgs.path} {};
          fixPaths = opt: opt // {
            declarations = map (d: "src/nix" + (lib.strings.removePrefix (toString ${src}) (toString d))) opt.declarations;
          };
          inherit (pkgs) lib;
          composition = pkgs.callPackage ${src}/eval-service.nix {} { modules = []; host = {}; name = abort "The manual's service options section must not depend on the service name."; };
      in map fixPaths (lib.filter (opt: opt.visible && !opt.internal) (lib.optionAttrSetToDocList composition.options))
    '';
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
    '';
    shellHook = ''
      live-build() {
        patchPhase
        inotifywait -e MODIFY -m -r . | while read; do
          make
        done
      }
    '';
    passthru = {
      inherit generatedDocBook;
    };
  };
in
  manual
