/* run.config
STDOPT: -acsl-import-addon-ensures-and-exits -then -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */


//@ \acsl_importer::ensures_and_exits p: c==0? \exit_status==1 : \result==1;
int may_exits(int c);
