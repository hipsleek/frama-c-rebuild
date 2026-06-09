/* run.config
STDOPT: -no-unicode  -acsl-import-addon-integer-cast -acsl-import %{dep:./@PTEST_NAME@.acsl} -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */

typedef struct S {
  int f ;
} S;

S const s;

/*@ ghost static const struct S foo; */

int job(int x) {
  /*@ assert foo.f == 2; */
  return x + s.f;
}
