{ lib
, stdenv
, black
, clang
, combinetura
, frama-c-hdrck
, frama-c-lint
, git
, git-lfs
, gnumake
, headache
, jq
, ocp-indent
, typos
} :
stdenv.mkDerivation rec {
  name = "frama-c-checkers-shell";
  buildInputs = [
    black
    clang
    combinetura
    frama-c-hdrck
    frama-c-lint
    git
    git-lfs
    gnumake
    headache
    jq
    ocp-indent
    typos
  ];
}
