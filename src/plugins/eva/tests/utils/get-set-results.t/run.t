Tests functions [get_results] and [set_results] from Eva_results module.
See test_get_set_results.ml for more details.

Compilation of the test_get_set_results.ml file
  $ dune build --cache=disabled --root . _build/default/test_get_set_results.cmxs

Load test_get_set_results.cmxs to test [get_results] and [set_results] on "file.i".
  $ frama-c -no-autoload-plugins -load-module eva,inout,scope,test_get_set_results.cmxs -eva-verbose 0 file.i
  [kernel] Parsing file.i (no preprocessing)
  Analyzing from precise call…
  Analyzing from imprecise call…
  [eva:alarm] file.i:11: Warning: check got status unknown.
  [eva:alarm] file.i:12: Warning: accessing out of bounds index. assert y < 40;
  Results from precise call:
    Callers of test: precise
    Values at end of function test:
      x: {4; 8; 12}
      y: {9; 17; 25}
    Properties:
      check y < 40;: VALID according to [ Eva ]
  Results from imprecise call:
    Callers of test: imprecise
    Values at end of function test:
      x: [1..20]
      y: [3..39],1%2
    Properties:
      check y < 40;: unknown (tried by [ Eva ])
      assert Eva: index_bound: y < 40;: unknown (tried by [ Eva ])
