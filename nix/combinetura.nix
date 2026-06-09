{ stdenv
, bisect_ppx
, dune_3
, findlib
, ocaml
, ppxlib
, xml-light
} :

stdenv.mkDerivation rec {
  pname = "combinetura";
  version = src.version;

  src = (import ./sources.nix {}).combinetura;

  buildInputs = [
    bisect_ppx
    dune_3
    findlib
    ocaml
    xml-light
  ];

  buildPhase = ''
    dune build @install
  '' ;

  installPhase = ''
    dune install --prefix $out --libdir $OCAMLFIND_DESTDIR $pname --docdir $out/share/doc --mandir $out/share/man
  '' ;
}
