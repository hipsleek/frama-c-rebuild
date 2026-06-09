{ mk_tests } :

mk_tests {
  tests-name = "plugins-tests";
  tests-command = ''
    dune exec -- frama-c-ptests -never-disabled tests src/plugins/*/tests
    dune build -j1 \
      @tests/callgraph/ptests \
      @tests/constant_propagation/ptests \
      @tests/impact/ptests \
      @tests/metrics/ptests \
      @tests/occurrence/ptests \
      @tests/pdg/ptests \
      @tests/slicing/ptests \
      @tests/scope/ptests \
      @tests/sparecode/ptests \
      @src/plugins/acsl-importer/tests/ptests \
      @src/plugins/aorai/tests/ptests \
      @src/plugins/alias/tests/ptests \
      @src/plugins/dive/tests/ptests \
      @src/plugins/instantiate/tests/ptests \
      @src/plugins/loop_analysis/tests/ptests \
      @src/plugins/markdown-report/tests/ptests \
      @src/plugins/nonterm/tests/ptests \
      @src/plugins/report/tests/ptests \
      @src/plugins/region/tests/ptests \
      @src/plugins/rte/tests/ptests \
      @src/plugins/server/tests/ptests \
      @src/plugins/volatile/tests/ptests
    dune runtest -j1 \
      src/plugins/server
  '';
  has-wp-proofs = true ;
}
