  $ dune build --cache=disabled --root . @install

  $ dune runtest --cache=disabled
  Running Crowbar tests on test_ghost_cfg
  ghost cfg: PASS
  

  $ find _build/default/failed_cases -name '*.i' -not -empty -exec cat '{}' ';' -exec frama-c -no-autoload-plugins '{}' ';'
