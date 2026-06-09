/* run.config
  STDOPT: #"-eva-no-alloc-returns-null"
*/
// This test includes specifications from several headers.

#include <stdlib.h>
#include <string.h>

volatile int nondet;
int main() {
  char *p = malloc(0);
  char *q = malloc(0);
  if (nondet) memcmp(p, q, 1); // invalid
}
