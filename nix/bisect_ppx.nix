{
  lib,
  fetchFromGitHub,
  fetchpatch,
  buildDunePackage,
  cmdliner,
  ppxlib,
  ocaml
}:

let
  param =
    if lib.versionAtLeast ocaml.version "5.04" then
      {
        version = "2.8.3+dev";
        owner = "frama-ci-bot";
        repo = "bisect_ppx";
        rev = "ocaml-5.4";
        hash = "sha256-XrM3Ka/u9D1xcsAbL7QogVl8z+XLY7muHVuRpX6XgKo=";
      }
    else
      {
        version = "2.8.3";
        owner = "aantron";
        repo = "bisect_ppx";
        rev = "2.8.3";
        hash = "sha256-3qXobZLPivFDtls/3WNqDuAgWgO+tslJV47kjQPoi6o=";
      };
in

buildDunePackage rec {
  pname = "bisect_ppx";
  inherit (param) version;

  src = fetchFromGitHub {
    inherit (param) owner repo rev hash;
  };

  minimalOCamlVersion = "4.11";

  buildInputs = [
    cmdliner
    ppxlib
  ];

  meta = {
    description = "Bisect_ppx is a code coverage tool for OCaml and Reason. It helps you test thoroughly by showing what's not tested";
    homepage = "https://github.com/aantron/bisect_ppx";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ momeemt ];
    mainProgram = "bisect-ppx-report";
  };
}
