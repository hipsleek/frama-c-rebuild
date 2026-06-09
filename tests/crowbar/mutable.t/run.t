  $ dune build --cache=disabled --root . @install

  $ dune runtest --cache=disabled --root .
  Running Crowbar tests on mutable
  mutable typeOffset: PASS
  

  $ dune build --cache=disabled --root . _build/default/mutable_const_fail.cmxs

  $ dune build --cache=disabled --root . _build/default/mutable_mutable_fail.cmxs

This produces an output when the test generated an error
  $ ./failed_test.sh
