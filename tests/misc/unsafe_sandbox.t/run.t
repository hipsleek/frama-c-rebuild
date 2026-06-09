  $ FRAMAC_SANDBOX=1 frama-c empty.c -no-autoload-plugins
  [kernel] Parsing empty.c (with preprocessing)
  $ FRAMAC_SANDBOX=1 frama-c empty.c -no-autoload-plugins -cpp-command "echo 'taking control';"
  [kernel] User Error: -cpp-command cannot be used in sandbox mode.
  [kernel] Frama-C aborted: invalid user input.
  [1]
  $ FRAMAC_SANDBOX=1 frama-c empty.c -no-autoload-plugins -cpp-extra-args "empty.c ; echo 'taking control';"
  [kernel] User Error: -cpp-extra-args cannot be used in sandbox mode.
  [kernel] Frama-C aborted: invalid user input.
  [1]
