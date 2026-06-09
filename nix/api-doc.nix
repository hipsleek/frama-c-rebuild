{ lib
, stdenv
, frama-c
, pandoc
, odoc
} :

stdenv.mkDerivation rec {
  pname = "api-doc";
  version = frama-c.version;
  slang = frama-c.slang;

  src = frama-c.build_dir + "/dir.tar";
  sourceRoot = ".";

  buildInputs = frama-c.buildInputs ++ [
    pandoc
    odoc
  ];

  buildPhase = ''
    dune build -j1 --error-reporting=twice @doc
    cp -r _build/default/_doc/_html frama-c-api
    echo ".dummy" > excluded
    tar czf frama-c-api.tar.gz -X excluded --owner=0 --group=0 --numeric-owner --sort=name --mtime="$(date --iso-8601 --date "today 00:00:00") 00:00Z" frama-c-api

    make server-doc NO_BUILD_FRAMAC=yes
    cp -r doc/server frama-c-server-api
    tar czf frama-c-server-api.tar.gz frama-c-server-api
  '';

  installPhase = ''
    mkdir -p $out
    cp frama-c-api.tar.gz $out
    cp frama-c-server-api.tar.gz $out
  '';
}
