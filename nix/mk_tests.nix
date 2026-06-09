# This template is meant to execute Frama-C tests
#
# Input variables:
#
# - tests-name (mandatory):
#   The name used for the derivation.
#
# - tests-command (mandatory):
#   The tests command to execute, generally something like:
#   ''
#     dune exec -- frama-c-ptests -never-disabled tests src/plugins/e-acsl/tests
#     dune build -j1 @src/plugins/e-acsl/tests/ptests
#   ''
#
# - has-wp-proofs (optional, defaults to 'false')
#   Indicates whether the tests execute WP proofs, if it the case the derivation
#   receives an additional build-input 'alt-ergo'. Furthermore, it export the WP
#   global cache and associated variables. This can be disabled in a test suite
#   by overriding the variables.
#
# - cover (optional, defaults to 'true')
#   Indicates whether the tests should generate coverage files. BEWARE! If you
#   disable this, make sure that you use a Frama-C version built without
#   coverage instrumentation, else Frama-C might be build several times during
#   the pipeline. See for example default-config-tests.nix

{ lib
, alt-ergo
, cvc4
, clang
, check-jsonschema
, dos2unix
, frama-c
, jq
, perl
, pkgs
, python3Packages
, socat
, stdenvNoCC
, time
, unixtools
, which
, wp-cache
, yq
} :

{ tests-name
, tests-command
, has-wp-proofs ? false
, cover ? true
, additional-build-inputs ? []
} :

stdenvNoCC.mkDerivation {
  pname = tests-name ;
  version = frama-c.version;
  slang = frama-c.slang;

  src = frama-c.build_dir + "/dir.tar";
  sourceRoot = ".";

  buildInputs = frama-c.buildInputs ++ [
    clang
    check-jsonschema
    dos2unix
    frama-c
    jq
    perl
    pkgs.fontconfig
    python3Packages.jsonschema
    python3Packages.pyaml
    socat
    time
    unixtools.getopt
    which
    yq
  ]
  ++ (if has-wp-proofs then [ alt-ergo cvc4 ] else [])
  ++ additional-build-inputs;

  postPatch = ''
    patchShebangs .
  '' ;

  # Keep main configuration
  configurePhase = ''
    true
  '';

  wp_cache =
    if has-wp-proofs
    then wp-cache.src
    else "" ;

  preBuild =
    ''
    mkdir home
    export HOME=$(pwd)/home
    export FONTCONFIG_FILE="${pkgs.fontconfig.out}/etc/fonts/fonts.conf"
    export FONTCONFIG_PATH="${pkgs.fontconfig.out}/etc/fonts/"
    '' +
    (if has-wp-proofs
     then ''
         export FRAMAC_WP_CACHE=offline
         export FRAMAC_WP_CACHEDIR=$wp_cache
     ''
     else "") +
    (if cover
     then ''
         mkdir -p _bisect
         export DUNE_WORKSPACE="$(pwd)/dev/dune-workspace.cover"
         export BISECT_FILE="$(pwd)/_bisect/bisect-"
     ''
     else "");

  postBuild =
    if cover
    then ''
      bisect-ppx-report cobertura --coverage-path=_bisect coverage-$pname.xml
      tar cfJ coverage.tar.xz coverage-$pname.xml
    ''
    else "" ;

  # The export NIX_GCC_DONT_MANGLE_PREFIX_MAP is meant to disable the
  # transformation of the path of Frama-C into uppercase when using the
  # __FILE__ macro.
  buildPhase = ''
    runHook preBuild
    export NIX_GCC_DONT_MANGLE_PREFIX_MAP=
  '' +
  tests-command + ''
    runHook postBuild
  '';

  # No installation required
  installPhase =
    if cover
    then ''
      mkdir -p $out
      cp -r coverage.tar.xz $out/$pname.tar.xz
    ''
    else ''
      touch $out
    '';
}
