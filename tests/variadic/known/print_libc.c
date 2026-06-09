/* run.config
   LOG: @PTEST_NAME@.pretty.c
   OPT: %{dep:./empty.c} -no-print-libc -print -ocode ./@PTEST_NAME@.pretty.c -then ./@PTEST_NAME@.pretty.c
 */

#include <stdio.h>

int main() {
  printf("");
}
