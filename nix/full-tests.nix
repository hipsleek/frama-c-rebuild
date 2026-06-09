# Not meant to be used in CI: uses maximum parallelism

{ mk_tests } :

mk_tests {
  tests-name = "full-tests";
  tests-command = ''
    dune exec -- frama-c-ptests -never-disabled tests src/plugins/*/tests
    dune build @ptests
  '';
  has-wp-proofs = true ;
}
