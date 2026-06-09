/* run.config
   STDOPT: -acsl-import-parse-only -kernel-warn-key annot-error=active -acsl-import-addon-ensures-and-exits -acsl-import %{dep:./parse-and-typing-only.acsl} -then -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
   STDOPT: -acsl-import-type-only -kernel-warn-key annot-error=active -acsl-import-addon-ensures-and-exits -acsl-import %{dep:./parse-and-typing-only.acsl} -then -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 EXIT: 1
   STDOPT: -acsl-import-addon-ensures-and-exits -acsl-import %{dep:./parse-and-typing-only.acsl} -then -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */

//@ \acsl_importer::ensures_and_exits p: c==0? \exit_status==1 : \result==1;
int may_exits(int c);

void may_exits_bis(int c) {
  may_exits(c);
}
