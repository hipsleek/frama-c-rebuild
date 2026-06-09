{
  mk_tests
, crowbar
} :

mk_tests {
  tests-name = "crowbar-tests";
  tests-command = ''
    CROWBAR= dune runtest tests/crowbar
  '';
  additional-build-inputs = [ crowbar ];
}
