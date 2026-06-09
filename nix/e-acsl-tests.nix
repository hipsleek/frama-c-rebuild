{ mk_tests } :

mk_tests {
  tests-name = "e-acsl-tests" ;
  tests-command = ''
    dune exec -- frama-c-ptests -never-disabled tests src/plugins/e-acsl/tests
    dune build -j1 @src/plugins/e-acsl/tests/ptests
  '';
}
