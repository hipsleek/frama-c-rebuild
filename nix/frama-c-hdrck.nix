{ lib
, stdenv
, dune_3
, gitignoreSource
, ocaml
} :

stdenv.mkDerivation rec {
  pname = "frama-c-hdrck";
  version =
    lib.strings.replaceStrings ["~"] ["-"]
      (lib.strings.removeSuffix "\n"
        (builtins.readFile ../VERSION));
  slang = lib.strings.removeSuffix "\n" (builtins.readFile ../VERSION_CODENAME);

  src = gitignoreSource ./../tools/hdrck ;

  buildInputs = [
    dune_3
    ocaml
  ];

  configurePhase = ''
    true
  '';

  buildPhase = ''
    dune build --root .
  '';

  installPhase = ''
    dune install --prefix $out --root .
  '';
}
