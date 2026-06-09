{ mk_tests, frama-c-no-cover } :

let mk_tests_distrib = mk_tests.override {
  frama-c = frama-c-no-cover ;
}; in
mk_tests_distrib {
  cover = false ;
  tests-name = "src-distrib-tests" ;
  tests-command = ''
    dune exec -- frama-c-ptests -never-disabled tests src/plugins/*/tests
    dune build -j1 @ptests_config
  '' ;
}
