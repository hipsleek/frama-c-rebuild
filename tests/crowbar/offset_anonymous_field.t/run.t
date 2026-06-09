  $ dune build --cache=disabled --root . @install

  $ dune runtest --cache=disabled --root .
  Running Crowbar tests on offset_anonymous_field
  designator and anonymous fields: PASS
  
This produces an output when the test generated an error
  $ find _build/default/failed_cases -name '*.i' -not -empty -exec cat '{}' ';' -exec frama-c -no-autoload-plugins '{}' ';'

