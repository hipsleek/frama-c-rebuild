{ mk_tests, config } :

let eva-tests = "eva-tests" + (if config == "" then "" else "-" + config); in
let ptests = "ptests_config" + (if config == "" then "" else "_" + config) ; in
let eva-test-dir = "src/plugins/eva/tests" ; in
let eva-test-target = "@${eva-test-dir}/${ptests}" ; in

# Only run cram tests on the default configuration.
let cram-tests-cmd = "dune runtest -j1 src/plugins/eva"; in
let eva-cram-tests = if config == "" then cram-tests-cmd else ""; in

mk_tests {
  tests-name = eva-tests ;
  tests-command = ''
    dune exec -- frama-c-ptests -never-disabled ${eva-test-dir}
    dune build -j1 ${eva-test-target}
    ${eva-cram-tests}
  '';
}
