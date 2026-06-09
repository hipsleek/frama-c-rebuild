{ lib
, stdenv
, frama-c
, pandoc
, odoc
} :

stdenv.mkDerivation rec {
  pname = "api-json-doc";
  version = frama-c.version;
  slang = frama-c.slang;

  src = frama-c.build_dir + "/dir.tar";
  sourceRoot = ".";

  buildInputs = frama-c.buildInputs ++ [
    pandoc
    odoc
  ];

  buildPhase = ''
    dune build -j1 --error-reporting=twice @doc-json
    cp -r _build/default/_doc/_html frama-c-api-json
    echo ".dummy" > excluded
    tar czf frama-c-api-json.tar.gz -X excluded --owner=0 --group=0 --numeric-owner --sort=name --mtime="$(date --iso-8601 --date "today 00:00:00") 00:00Z" frama-c-api-json
  '';

  installPhase = ''
    mkdir -p $out
    cp frama-c-api-json.tar.gz $out
  '';
}
