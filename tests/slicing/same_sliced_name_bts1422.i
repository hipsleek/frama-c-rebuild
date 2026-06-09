/* run.config
STDOPT: +"-main foo -slice-value y -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check "
*/

int y;

void foo(int x);

void foo(int x) { x++; y++; }

void (*ptr)(int x) = &foo;
