/* run.config*
 MODULE: @PTEST_NAME@
 OPT: -test-decimal-option 0.12 -test-deprecated-option 0.12
 OPT: -autocomplete test
 OPT: -test-h
 EXIT: 1
 OPT: -test-decimal-option -1.0
 OPT: -test-decimal-option 2.0
 OPT: -test-decimal-option x
*/

/* Just build and run the module */
