/*  run.config
STDOPT: +"-slice-calls main -then-on 'Slicing export' -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i"
STDOPT: +"-slice-calls f -main f -then-on 'Slicing export' -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i"
*/
int x = 0;

int main() {
  while(1) 
    x=0;
  return x + 1;
}

int f() {
  while(1) 
    x=0;
  return x + 1;
}
