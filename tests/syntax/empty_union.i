/* run.config*
   STDOPT: +"-machdep gcc_x86_32 -print -ocode @PTEST_NAME@_@PTEST_NUMBER@_reparse.c -then @PTEST_NAME@_@PTEST_NUMBER@_reparse.c -ocode=''"
   STDOPT: +"-machdep msvc_x86_64 -print -ocode @PTEST_NAME@_@PTEST_NUMBER@_reparse.c -then @PTEST_NAME@_@PTEST_NUMBER@_reparse.c -ocode=''"
 EXIT: 1
   STDOPT:
 */

// based on GCC's 'torture' test suite
union empty {} eu = {};
