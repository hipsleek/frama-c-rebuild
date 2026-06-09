/* run.config
STDOPT: -acsl-import %{dep:./equals.acsl} -then -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */
int equals(unsigned int const *a, unsigned int n, unsigned int const *b) {
   unsigned int i;
   for (i = 0; i < n; i++)
      if (a[i] != b[i])
     return 0;
   return 1;
}
