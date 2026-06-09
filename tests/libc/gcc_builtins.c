/* run.config
   STDOPT: +"-machdep gcc_x86_64"
*/

#include <limits.h>

int main() {
  int ia = 4;
  int ib = 5;
  int ires;
  int overflow = __builtin_add_overflow(ia, ib, &ires);
  //@ assert !overflow;
  ia = INT_MAX;
  overflow = __builtin_add_overflow(ia, ib, &ires);
  //@ assert overflow;

  unsigned ua = 42, ub = 100, ures;
  long la = -10, lb = LONG_MIN, lres;
  unsigned long ula = ULONG_MAX-4, ulb = 7, ulres;
  long long lla = LLONG_MAX-5, llb = 10, llres;
  unsigned long long ulla = ULLONG_MAX, ullb = ULLONG_MAX, ullres;

  overflow = __builtin_add_overflow(ua, ub, &ures);
  //@ assert !overflow;
  overflow = __builtin_add_overflow(la, lb, &lres);
  //@ assert overflow;
  overflow = __builtin_add_overflow(ula, ulb, &ulres);
  //@ assert overflow;
  overflow = __builtin_add_overflow(lla, llb, &llres);
  //@ assert overflow;
  overflow = __builtin_add_overflow(ulla, ullb, &ullres);
  //@ assert overflow;

  overflow = __builtin_sub_overflow(ua, ub, &ures);
  //@ assert overflow;
  overflow = __builtin_sub_overflow(la, lb, &lres);
  //@ assert !overflow;
  overflow = __builtin_sub_overflow(ula, ulb, &ulres);
  //@ assert !overflow;
  overflow = __builtin_sub_overflow(lla, llb, &llres);
  //@ assert !overflow;
  overflow = __builtin_sub_overflow(ulla, ullb, &ullres);
  //@ assert !overflow;

  overflow = __builtin_mul_overflow(ua, ub, &ures);
  //@ assert !overflow;
  overflow = __builtin_mul_overflow(la, lb, &lres);
  //@ assert overflow;
  overflow = __builtin_mul_overflow(ula, ulb, &ulres);
  //@ assert overflow;
  overflow = __builtin_mul_overflow(lla, llb, &llres);
  //@ assert overflow;
  overflow = __builtin_mul_overflow(ulla, ullb, &ullres);
  //@ assert overflow;

  ia = ib = ires = ua = ub = ures = la = lb = lres = ula = ulb = ulres =
    lla = llb = llres = ulla = ullb = ullres = 0;
}
