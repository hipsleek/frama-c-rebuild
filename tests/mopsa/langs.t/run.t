Test inclusion of "non-C" languages (C++), with a 'fake parser'.
We compile a module that is then loaded by Frama-C
  $ dune build --cache=disabled --root . @install
  $ dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-plugin mopsa-langs -mopsa-db mopsa-db.json -mopsa-target a.out
  [kernel] adding .cc to known file extensions
  [kernel:mopsa-db:non-c-source] Warning: 
    ignoring non-C (FOOBAR) dependency: cpp.foo
    (setting this warning category to inactive or feedback will try to parse it nevertheless)
  [kernel] Parsing cpp.cc (external front-end)
We will 'force' parsing of a non-source file (foo.bar).
We omit the 'linker input unused' message emitted by GCC/Clang.
  $ dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-plugin mopsa-langs -mopsa-db mopsa-db.json -mopsa-target a.out -kernel-warn-key mopsa-db:non-c-source=inactive 2>&1 | grep -v "linker.*input"
  [kernel] adding .cc to known file extensions
  [kernel] Parsing cpp.cc (external front-end)
  [kernel] Parsing foo.bar (with preprocessing)
