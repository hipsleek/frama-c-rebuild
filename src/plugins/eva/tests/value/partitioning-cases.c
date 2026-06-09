/* run.config*

   STDOPT: #"-eva-partition-history 0"
   STDOPT: #"-eva-partition-history 1"
*/

#include "__fc_builtin.h"
#include <stdlib.h>

void test_builtin(void)
{
  int *p = malloc(1);
  Frama_C_show_each(p);
}

/*@ assigns \result \from nb;
    behavior neg:
      assumes nb < 0;
      ensures \result == -1;
    behavior pos:
      assumes nb > 0;
      ensures \result == 1;
    behavior zero:
      assumes nb == 0;
      ensures \result == 0;
    complete behaviors;
    disjoint behaviors;
*/
int sgn_spec(int nb);

/*@ assigns \result \from x;
    ensures \result < 0 || \result > 0;
*/
int f_spec(int x);

void test_spec(void)
{
  int res = sgn_spec(Frama_C_interval(-100, 100));
  Frama_C_show_each(res);
  res = f_spec(Frama_C_interval(0, 100));
  Frama_C_show_each(res);
}

void test_assert(void)
{
  int nb = Frama_C_interval(0, 100);
  //@ assert nb < 50 || 50 <= nb;
  Frama_C_show_each(nb);
}

void main(void)
{
  test_builtin();
  test_spec();
  test_assert();
}
