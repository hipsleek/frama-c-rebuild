/* run.config
   COMMENT: cf issue framac#1119
*/

#include <limits.h>
#include <stddef.h>

int find_last_of(int const *a, int len, int value) {
  //@ ghost size_t o = len ;
  //@ loop invariant \forall integer i ; len <= i < o ==> a[i] != value ;
  while (len) {
    len--;
  }
  return INT_MAX;
}
int main(void) {
  int a[1] = {1};
  find_last_of(a, 1, 0);
}
