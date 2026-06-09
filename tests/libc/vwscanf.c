#include "wchar.c"
#include "__fc_builtin.h"

volatile int nondet;

int caller_stub_for_vwscanf(const wchar_t * restrict format, ...);

int main() {
  int d;
  char c;
  long double Ld;
  char s[30];
  ptrdiff_t t;
  intmax_t j;
  size_t z;
  wchar_t lc;
  int res = caller_stub_for_vwscanf(L"%+d %-2c % 41.999Lf %s %ti %jx %zu %lc", &d, &c, &Ld, s, &t, &j, &z, &lc);
  if (res == 4) {
    //@ check \initialized(&d);
    //@ check \initialized(&s);
    Frama_C_show_each_must_be_reachable(d, &c, &Ld, s, t, j, z, lc);
  }

  return 0;
}

int caller_stub_for_vwscanf(const wchar_t * restrict format, ...) {
  va_list args;
  va_start(args, format);
  int res = vwscanf(format, args);
  va_end(args);
  return res;
}
