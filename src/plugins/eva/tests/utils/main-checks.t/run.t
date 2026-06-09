Test error messages when a builtin or a specification is used for the main function.

Eva should not crash when a builtin is used for the main function.
  $ frama-c -no-autoload-plugins -load-module eva,inout,scope main_checks.i -eva -main strlen
  [kernel] Parsing main_checks.i (no preprocessing)
  [eva] Analyzing a complete application starting at strlen
  [eva:initial-state] Values of globals at initialization
    
  [eva:builtins:missing-spec] main_checks.i:8: Warning: 
    No Frama-C libc specification found for function strlen, for which a builtin is used; its soundness relies on the specification provided by the user.
  [eva] User Error: Cannot analyze program from main function strlen, for which a builtin is used.
  [kernel] Plug-in eva aborted: invalid user input.
  [1]


Mthread should not crash when a specification is used for the main function.
  $ frama-c -no-autoload-plugins -load-module eva,inout,scope main_checks.i -eva -mthread -eva-use-spec main
  [mt] Preparing sources for Mthread with builtins only
  [kernel] Parsing FRAMAC_SHARE/mt/mthread.c (with preprocessing)
  [kernel] Parsing main_checks.i (no preprocessing)
  [mt] Warning: Mthread is an experimental plugin and is still in development.
  [kernel] Plug-in mt aborted: unimplemented feature.
    You may send a feature request at https://git.frama-c.com/pub/frama-c/issues with:
    '[Plug-in mt] Using an ACSL specification or a builtin to interpret entry point main of thread <main> is not supported.'.
  [3]

Mthread should not crash when the main function has no body.
  $ frama-c -no-autoload-plugins -load-module eva,inout,scope main_checks.i -eva -mthread -main spec_only
  [mt] Preparing sources for Mthread with builtins only
  [kernel] Parsing FRAMAC_SHARE/mt/mthread.c (with preprocessing)
  [kernel] Parsing main_checks.i (no preprocessing)
  [mt] Warning: Mthread is an experimental plugin and is still in development.
  [kernel] Plug-in mt aborted: unimplemented feature.
    You may send a feature request at https://git.frama-c.com/pub/frama-c/issues with:
    '[Plug-in mt] Using an ACSL specification or a builtin to interpret entry point spec_only of thread <main> is not supported.'.
  [3]

Mthread should not crash when a builtin is used for the main function.
  $ frama-c -no-autoload-plugins -load-module eva,inout,scope main_checks.i -eva -mthread -main strlen
  [mt] Preparing sources for Mthread with builtins only
  [kernel] Parsing FRAMAC_SHARE/mt/mthread.c (with preprocessing)
  [kernel] Parsing main_checks.i (no preprocessing)
  [mt] Warning: Mthread is an experimental plugin and is still in development.
  [kernel] Plug-in mt aborted: unimplemented feature.
    You may send a feature request at https://git.frama-c.com/pub/frama-c/issues with:
    '[Plug-in mt] Using an ACSL specification or a builtin to interpret entry point strlen of thread <main> is not supported.'.
  [3]
