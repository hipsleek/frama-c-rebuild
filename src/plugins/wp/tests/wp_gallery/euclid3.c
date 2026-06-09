#include <limits.h>

//@ import why3: number::Gcd;

//@ import why3: int::MinMax;

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

/*@
    requires \abs(a) <= INT_MAX;
    requires \abs(b) <= INT_MAX;
    requires \abs(c) <= INT_MAX;
    requires \abs(d) <= INT_MAX;
    assigns \nothing;
    ensures \result == Gcd::gcd(MinMax::min(a,b), MinMax::max(c,d) );
*/
int minmax_gcd(int a, int b, int c, int d)
{
    int x = (a > b) ? b : a;
    int y = (c > d) ? c : d;
    return euclid_gcd(x,y);
}
