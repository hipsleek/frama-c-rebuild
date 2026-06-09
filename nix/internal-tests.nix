# Note: plugins are loaded from 'internal-tests.sh'

# Nix
{ lib
, stdenvNoCC # for E-ACSL
, fetchurl
, gitignoreSource
, makeWrapper
, nix-gitignore
, writeText
# Generic
, findlib
# Frama-Clang
, cmake
, camlp5
, camlp-streams
, gnused
, llvmPackages
, pcre2
# Frama-C build
, apron
, camlzip
, clang
, dune_3
, dune-configurator
, dune-site
, fpath
, gcc14
, graphviz
, menhir
, menhirLib
, ocaml
, ocamlgraph
, ppx_deriving
, ppx_deriving_yaml
, ppx_deriving_yojson
, ppx_inline_test
, unionFind
, yojson
, why3
, yaml
, zarith
, zmq
# Frama-C tests
, alt-ergo
, check-jsonschema
, dos2unix
, jq
, perl
, pkgs
, python310
, python3Packages
, socat
, swi-prolog
, time
, unixtools
, which
, wp-cache
, yq
}:

# We do not use buildDunePackage because Frama-C still uses a Makefile to build
# some files and prepare some information before starting dune.
stdenvNoCC.mkDerivation rec {
  pname = "frama-c-internal-tests";
  version =
    lib.strings.replaceStrings ["~"] ["-"]
      (lib.strings.removeSuffix "\n"
        (builtins.readFile ../VERSION));
  slang = lib.strings.removeSuffix "\n" (builtins.readFile ../VERSION_CODENAME);

  src = gitignoreSource ./..;

  # Prevent CMake from doing stuff without being asked
  dontUseCmakeConfigure=true;

  nativeBuildInputs = [
    which
  ];

  buildInputs = [
    apron
    camlp5
    camlp-streams
    camlzip
    clang
    cmake
    dune_3
    dune-configurator
    dune-site
    findlib
    fpath
    gcc14
    gnused
    graphviz
    llvmPackages.llvm.dev
    llvmPackages.clang-unwrapped.dev
    menhir
    menhirLib
    ocaml
    ocamlgraph
    pcre2
    ppx_deriving
    ppx_deriving_yaml
    ppx_deriving_yojson
    ppx_inline_test
    unionFind
    yojson
    which
    why3
    yaml
    zarith
    zmq
    # Tests
    alt-ergo
    check-jsonschema
    dos2unix
    jq
    perl
    pkgs.fontconfig
    python310
    python3Packages.jsonschema
    python3Packages.pyaml
    socat
    swi-prolog
    time
    unixtools.getopt
    which
    yq
  ];

  outputs = [ "out" ];

  preConfigure = ''
    dune build @frama-c-configure
  '';

  # Do not use default parallel building, but allow 2 cores for Frama-C build
  enableParallelBuilding = false;
  buildPhase = ''
    dune build -j2 --error-reporting=twice @install
    make tools/ptests/ptests.exe
    make tools/ptests/wtests.exe
  '';

  wp_cache = wp-cache.src ;

  doCheck = true;
  preCheck = ''
    patchShebangs .
    mkdir home
    export HOME=$(pwd)/home
    export FONTCONFIG_FILE="${pkgs.fontconfig.out}/etc/fonts/fonts.conf"
    export FONTCONFIG_PATH="${pkgs.fontconfig.out}/etc/fonts/"
    export FRAMAC_WP_CACHE=offline
    export FRAMAC_WP_CACHEDIR=$wp_cache
  '';

  # The export NIX_GCC_DONT_MANGLE_PREFIX_MAP is meant to disable the
  # transformation of the path of Frama-C into uppercase when using the
  # __FILE__ macro.
  checkPhase = ''
    runHook preCheck
    export NIX_GCC_DONT_MANGLE_PREFIX_MAP=
    dune exec -- frama-c-ptests -never-disabled tests src/plugins/*/tests
    dune build -j1 @ptests_config
  '';

  installFlags = [
    "PREFIX=$(out)"
  ];

  meta = {
    description = "An extensible and collaborative platform dedicated to source-code analysis of C software";
    homepage = "http://frama-c.com/";
    license = lib.licenses.lgpl21;
    platforms = lib.platforms.unix;
  };
}
