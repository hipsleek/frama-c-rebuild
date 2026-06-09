/* run.config_qualif
   DONTRUN:
*/

#include <stdint.h>

#define SIZE (SIZE_MAX / sizeof(int))

/*@
  assigns \result \from p[0..n-1];
*/
int f(int *p, int n)
{
  int s = 0;
  int tmp[SIZE];
  /*@ loop assigns i,s, tmp[..];
    @ loop variant n - i;
    @ */
  for (int i = 0; i < n; i++) {
    s+= p[i]; tmp[i] = s;
  }
  return s;
}
