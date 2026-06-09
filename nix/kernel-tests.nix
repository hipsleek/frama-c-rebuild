{ mk_tests } :

mk_tests {
  tests-name = "kernel-tests";
  tests-command = ''
    dune exec -- frama-c-ptests -never-disabled tests
    dune build -j1 \
      @tests/cil/ptests \
      @tests/jcdb/ptests \
      @tests/libc/ptests \
      @tests/misc/ptests \
      @tests/pretty_printing/ptests \
      @tests/saveload/ptests \
      @tests/spec/ptests \
      @tests/syntax/ptests \
      @tests/variadic/ptests \
      @src/kernel_internals/parsing/tests/ptests
    dune runtest -j1 \
      tests
    dune build @runtest-frama_c_kernel
    make -C share/machdeps check-schema
  '';
}
