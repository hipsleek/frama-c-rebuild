/* run.config
  MACRO: PATHNAME  #"%{dep:@PTEST_SUITE_DIR@/../pdg/variadic.c}"
    STDOPT: @PATHNAME@ +"-slice-return f3 -no-slice-callers -then-on 'Slicing export' -print"
    STDOPT: @PATHNAME@ +"-slice-return f3 -no-slice-callers -no-variadic-translation -then-last -print"
    STDOPT: @PATHNAME@ +"-slice-return f3 -then-on 'Slicing export' -print"
    STDOPT: @PATHNAME@ +"-slice-return main -then-on 'Slicing export' -print"
    STDOPT: @PATHNAME@ +"-slice-return main -slicing-level 3  -then-on 'Slicing export' -print"
*/
