  $ dune build --cache=disabled --root . @install

  $ dune runtest --cache=disabled --root .
  Running Crowbar tests on complete_type
  complete type: PASS
  
This produces an output when the above test generated an error
  $ find _build/default/failed_cases -name '*.i' -not -empty -exec cat '{}' ';' -exec frama-c -no-autoload-plugins '{}' ';'

