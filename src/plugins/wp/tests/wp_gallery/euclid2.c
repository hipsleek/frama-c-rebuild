#include <limits.h>

//@ import why3: number::Gcd;

/*@
  requires \abs(a) <= INT_MAX ;
  requires \abs(b) <= INT_MAX ;
  assigns \nothing;
  ensures \result == Gcd::gcd(a,b);
*/

int euclid_gcd(int a, int b)
{
  int r;
  /*@
    loop assigns a, b, r;
    loop invariant Gcd::gcd(a,b) == \at( Gcd::gcd(a,b), Pre );
    loop variant \abs(b);
  */
  while( b != 0 ) {
    r = b ;
    b = a % b ;
    a = r ;
  }
  return a < 0 ? -a : a;
}
