/* run.config
PLUGIN: @PTEST_PLUGIN@ eva,scope
STDOPT: -kernel-warn-key annot-error=active -acsl-import-keep-unused-symbols -acsl-import %{dep:./keep-unused-symbols.acsl} -then -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */

typedef signed int T_INT32;
typedef signed int T_unused;

T_INT32 f_unused_without_spec(T_INT32 A);
