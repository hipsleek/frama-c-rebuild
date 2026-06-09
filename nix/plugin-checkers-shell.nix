{ lib
, stdenv
, clang_18
, frama-c
, frama-c-hdrck
, frama-c-lint
, git
, gnumake
, headache
, ocp-indent
} :
stdenv.mkDerivation rec {
  name = "plugin-checkers-shell";
  buildInputs = [
    clang_18
    frama-c
    frama-c-hdrck
    frama-c-lint
    git
    gnumake
    headache
    ocp-indent
  ];
}
