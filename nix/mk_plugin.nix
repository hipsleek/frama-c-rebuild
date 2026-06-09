# This template is meant to build external plugins
#
# Input variables:
#
# - plugin-name (mandatory):
#   The name used for the derivation. It is also used for the name of the Opam
#   file during the INSTALL phase. However, it is only necessary if the plugin
#   is a dependency for another, so the installation of this file can be
#   disabled with the 'install-opam' variable described below.
#
# - plugin-src (mandatory):
#   The source files used for building, generally 'gitignoreSource ./..'. It
#   must be provided in the plugin because from this './..' is the Frama-C
#   directory
#
# - additional-build-inputs (optional, defaults to [])
#   Additional Nix packages that are added to the 'buildInput' variable,
#   originally it contains Frama-C + all its dependencies
#
# - additional-check-inputs (optional, defaults to [])
#   Additional Nix packages that are added to the 'checkInput' variable,
#   originally it contains 'time'
#
# - has-wp-proofs (optional, defaults to 'false')
#   Indicates whether the plugin execute WP proofs during tests, if it the case
#   the derivation receives an additional check-input 'alt-ergo'. Furthermore,
#   it exports the WP global cache and associated variables. This can be
#   disabled in a test suite by overriding the variables.
#
# - install-opam (optional, default to 'true')
#   Indicates whether the generated Opam file should be installed. Unless it is
#   not possible:
#   - if your plugin does not provide an Opam file or,
#   - *if the derivation name does not correspond to the name of the plugin*
#     (see for example the Frama-Clang plugin)
#   There is no reason to disable this.
#
# The plugin must have:
#   - a '@frama-c-configure' Dune rule
#   - a LICENSE file

{ lib
, stdenv
, alt-ergo
, frama-c
, time
, wp-cache
}:

{ plugin-name
, plugin-src
, additional-build-inputs ? []
, additional-check-inputs ? []
, has-wp-proofs ? false
, install-opam ? true
}:

stdenv.mkDerivation {
  name = plugin-name;
  src = plugin-src;

  buildInputs = frama-c.buildInputs ++ [
    frama-c
  ]
  ++ additional-build-inputs ;

  checkInputs = [
    time
  ]
  ++ (if has-wp-proofs then [ alt-ergo ] else [])
  ++ additional-check-inputs ;

  # Note: no check is performed, it is just used to show dependencies
  configurePhase = ''
    dune build @frama-c-configure
  '';

  # Do not use default parallel building, but allow 2 cores for Frama-C build
  enableParallelBuilding = false;

  # Some plugins have sh scripts during build
  preBuild  = ''
    patchShebangs .
  '';
  buildPhase = ''
    runHook preBuild
    dune build -j2 --error-reporting=twice @install
  '';

  wp_cache =
    if has-wp-proofs
    then wp-cache.src
    else "" ;

  doCheck = true;

  # Some plugins have sh scripts during check
  preCheck = ''
    patchShebangs .
  '' + (if has-wp-proofs then ''
    export FRAMAC_WP_CACHE=offline
    export FRAMAC_WP_CACHEDIR=$wp_cache
    ''
  else "") ;

  # The export NIX_GCC_DONT_MANGLE_PREFIX_MAP is meant to disable the
  # transformation of the path of Frama-C into uppercase when using the
  # __FILE__ macro.
  checkPhase = ''
    runHook preCheck
    dune exec -- frama-c-ptests -never-disabled tests
    export NIX_GCC_DONT_MANGLE_PREFIX_MAP=
    dune build -j1 @tests/ptests
  '';

  installFlags = [
    "PREFIX=$(out)"
  ];

  postInstall = if install-opam then ''
    cp frama-c-$name.opam $out/lib/frama-c-$name/opam
  '' else "" ;

  # Required so that tests of external plugins can be executed
  postFixup = ''
    cp -r $out/share/doc $out/doc
  '';
}
