{
  lib,
  fetchurl,
  fetchFromGitHub,
  buildDunePackage,
  ocaml,
  ocaml-compiler-libs,
  ocaml-migrate-parsetree,
  ppx_derivers,
  stdio,
  stdlib-shims,
  ocaml-migrate-parsetree-2,
}:

let
  param =
    if lib.versionAtLeast ocaml.version "5.04" then
      {
        version = "0.37.0";
        sha256 = "sha256-LiI4N+fOzDvISkMkMsCnL04dW+kWXJwzdy8VbbhdsLM=";
      }
    else
      {
        version = "0.35.0";
        sha256 = "sha256-2dlZ/J+EJgSH5FaE3HQYmKkvxVBrYaf1ysZdIYMtuSU=";
      };
in

buildDunePackage rec {
  pname = "ppxlib";
  inherit (param) version;

  src = fetchurl {
    url = "https://github.com/ocaml-ppx/ppxlib/releases/download/${version}/ppxlib-${version}.tbz";
    inherit (param) sha256;
  };

  propagatedBuildInputs = [
    ocaml-compiler-libs
    ppx_derivers
    stdio
    stdlib-shims
  ];

  meta = {
    description = "Comprehensive ppx tool set";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.vbgl ];
    homepage = "https://github.com/ocaml-ppx/ppxlib";
  };
}
