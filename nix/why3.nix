{ lib
, stdenv
, ocaml
, camlzip
, findlib
, menhir
, menhirLib
, ocamlgraph
, ppx_deriving
, ppx_sexp_conv
, re
, sexplib
, zarith
, coqPackages
, autoreconfHook
}:

stdenv.mkDerivation rec {
  pname = "why3";
  src = (import ./sources.nix {}).why3;
  version = src.version;

  nativeBuildInputs = [
    autoreconfHook
    ocaml
    findlib
    menhir
    coqPackages.coq
  ];

  buildInputs = [
    ocamlgraph
    ppx_deriving
    ppx_sexp_conv
    zarith
    coqPackages.coq
    coqPackages.flocq
  ];

  propagatedBuildInputs = [
    camlzip
    menhirLib
    re
    sexplib
    zarith
  ];

  enableParallelBuilding = true;

  configureFlags = [
    "--enable-verbose-make"
  ];

  outputs = [
    "out"
    "dev"
  ];

  installTargets = [
    "install"
    "install-lib"
  ];

  postInstall = ''
    mkdir -p $dev/lib
    mv $out/lib/ocaml $dev/lib/
  '';

  meta = with lib; {
    description = "Platform for deductive program verification";
    homepage = "https://why3.lri.fr/";
    license = licenses.lgpl21;
    platforms = platforms.unix;
    maintainers = with maintainers; [
      thoughtpolice
      vbgl
    ];
  };
}
