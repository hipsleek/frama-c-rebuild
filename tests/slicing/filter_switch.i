/* run.config
LOG: ./@PTEST_NAME@_@PTEST_NUMBER@.i
OPT: @EVA_OPTIONS@ -main main1 -slice-return main1 -then-on 'Slicing export' -print -ocode ./@PTEST_NAME@_@PTEST_NUMBER@.i -then ./@PTEST_NAME@_@PTEST_NUMBER@.i -no-deps 
LOG: ./@PTEST_NAME@_@PTEST_NUMBER@.i
OPT: @EVA_OPTIONS@ -main main2 -slice-return main2 -then-on 'Slicing export' -print -ocode ./@PTEST_NAME@_@PTEST_NUMBER@.i -then ./@PTEST_NAME@_@PTEST_NUMBER@.i -no-deps 
LOG: ./@PTEST_NAME@_@PTEST_NUMBER@.i
OPT: @EVA_OPTIONS@ -main main3 -slice-return main3 -then-on 'Slicing export' -print -ocode ./@PTEST_NAME@_@PTEST_NUMBER@.i -then ./@PTEST_NAME@_@PTEST_NUMBER@.i -no-deps 
*/

int main1(void) {
  int x = 0;
  int i = 1;

  switch (i) { // No selectable case but L: is reachable from goto
    case 10:
    L:
      x++;
  }

  if (i > x)
    goto L;
  
  return x;  
}

int main2(void) {
  int x = 0;
  int i = 1;

  switch (i) { // First case will be taken
    case 1:
      x++;
      break;

    case 10:
    L:
      x++;
      break;
  }

  if (i > x)
    goto L;
  
  return x;  
}

int main3(void) {
  int x = 0;
  int i = 2;

  switch (i) { // default case will be taken
    case 10:
    L:
      x++;
      break;
    default:
      x++;
      break;
  }

  if (i > x)
    goto L;
  
  return x;  
}
