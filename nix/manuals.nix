{ lib
, stdenv
, frama-c
, headache
, texlive
} :

stdenv.mkDerivation rec {
  pname = "manuals";
  version = frama-c.version;
  slang = frama-c.slang;


  build_dir = frama-c.build_dir + "/dir.tar";
  acsl = fetchGit {
    url = "https://github.com/acsl-language/acsl.git";
    name = "acsl";
  };
  srcs = [
    build_dir
    acsl
  ] ;

  sourceRoot = ".";

  buildInputs = frama-c.buildInputs ++ [
    frama-c
    headache
    texlive.combined.scheme-full
  ];

  postUnpack = ''
    mv acsl doc
  '' ;

  postPatch = ''
    patchShebangs .
  '' ;

  # Keep main configuration
  configurePhase = ''
    true
  '';

  buildPhase = ''
    make -C doc NO_SUFFIX=yes all version
  '';

  installPhase = ''
    mkdir -p $out
    cp ./doc/manuals/*.pdf $out
    cp ./doc/manuals/*.tar.gz $out
    cp ./doc/manuals/*.txt $out
  '';
}
