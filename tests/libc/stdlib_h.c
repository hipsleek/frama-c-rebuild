/* run.config
 STDOPT: +"-machdep msvc_x86_64"
 STDOPT: +"-machdep ppc_32"
 */
// Note: machdep MSVC is used to avoid warnings due to "non implemented long
// double" when testing strtold. In MSVC, "long double" is mapped to "double".
#include <stdlib.h>
#include <errno.h>
//@ assigns \result \from *(int*)a, *(int*)b;
int compare_int(const void *a, const void *b) {
  return (*(int*)a < *(int*)b) ? -1 : (*(int*)a > *(int*)b);
}

char putenv_buf[30] = "PATH3=/:/foo:/bar:";

volatile int nondet;
int main() {
  int base = nondet ? 0 : nondet ? 2 : 36;
  char *sl = "12 34 -56";
  char *s = sl;
  char *pl, *q;
  errno = 0;
  long l = strtol(s, &pl, base);
  //@ split errno;
  if (errno) return 1;
  l = strtol(pl, &q, base);
  //@ split errno;
  if (errno) return 1;
  l = strtol(q, NULL, base);
  //@ split errno;
  if (errno) return 1;
  l = strtol(s+9, NULL, base);
  //@ split errno;
  if (errno) return 1;

  s = sl;
  char *pll;
  long long ll = strtoll(s, &pll, base);
  //@ split errno;
  if (errno) return 1;
  ll = strtoll(pll, &q, base);
  //@ split errno;
  if (errno) return 1;
  ll = strtoll(q, NULL, base);
  //@ split errno;
  if (errno) return 1;

  s = sl;
  char *pul;
  unsigned long ul = strtoul(s, &pul, base);
  //@ split errno;
  if (errno) return 1;
  ul = strtoul(pul, &q, base);
  //@ split errno;
  if (errno) return 1;
  ul = strtoul(q, NULL, base);
  //@ split errno;
  if (errno) return 1;

  s = sl;
  char *pull;
  unsigned long long ull = strtoull(s, &pull, base);
  //@ split errno;
  if (errno) return 1;
  ull = strtoull(pull, &q, base);
  //@ split errno;
  if (errno) return 1;
  ull = strtoull(q, NULL, base);
  //@ split errno;
  if (errno) return 1;

  char *sd = " 3.14 0x1.2p2";
  s = sd;
  char *pd;
  double d = strtod(s, &pd);
  d = strtod(pd, &q);
  d = strtod(q, NULL);

  s = sd;
  char *pld;
  long double ld = strtold(s, &pld);
  ld = strtold(pld, &q);
  ld = strtold(q, NULL);

  s = sd;
  char *pf;
  float f = strtof(s, &pf);
  f = strtof(pf, &q);
  f = strtof(q, NULL);

  int ai[4] = {1, -1, 50000, 20};
  int key = 4;
  int *p = bsearch(&key, ai, 4, sizeof(int), compare_int);
  //@ assert p == \null;
  key = -1;
  p = bsearch(&key, ai, 4, sizeof(int), compare_int);
  //@ assert p == &ai[1];

  // tests for *env functions
  /*{
    char mutable[12] = "MUTABLE=yes";
    putenv(mutable);
    mutable[8] = 'n';
    mutable[9] = 'o';
    mutable[10] = 0;
    char *v = getenv("MUTABLE");
    if (v[8] != 'n') return 1; // possible only if imprecise
  }*/

  char tempFilename[] = "blaXXXXXX";
  int r = mkstemp(tempFilename);

  if (nondet) {
    // should fail: seed not initialized
    drand48();
  }
  if (nondet) {
    // should fail: seed not initialized
    lrand48();
  }
  if (nondet) {
    // should fail: seed not initialized
    mrand48();
  }
  unsigned short xsubi[3];
  if (nondet) {
    // should fail: xsubi
    erand48(xsubi);
  }
  xsubi[0] = 42;
  xsubi[1] = 42;
  xsubi[2] = 42;
  d = erand48(xsubi);
  //@ assert 0.0 <= d < 1.0;
  l = jrand48(xsubi);
  //@ assert -2147483648 <= l < 2147483648;
  l = nrand48(xsubi);
  //@ assert 0 <= l < 2147483648;

  srand48(42);
  unsigned short seed48v[3] = {0, 4, 2};
  unsigned short *res = seed48(seed48v);
  unsigned short param[7] = {0, 4, 2, 0, 4, 2, 0};
  lcong48(param);

  d = drand48();
  //@ assert 0.0 <= d < 1.0;
  l = mrand48();
  //@ assert -2147483648 <= l < 2147483648;
  l = lrand48();
  //@ assert 0 <= l < 2147483648;

  char *path = getenv("PATH");
  if (path) {
    //@ check imprecise: valid_read_string(path);
  }
  int setenv_res = setenv("PATH2", "/", 0);
  //@ check setenv_res == -1 || setenv_res == 0;
  int putenv_res = putenv(putenv_buf);

  div_t div_res;
  if (nondet) {
    div_res = div(1, 0);
    //@ check unreachable: \false;
  }
  if (nondet) {
    div_res = div(INT_MIN, -1);
    //@ check unreachable: \false;
  }
  div_res = div(INT_MAX, -1);

  ldiv_t ldiv_res = ldiv(LONG_MAX, -1);
  lldiv_t lldiv_res = lldiv(LLONG_MAX, -1);
  return 0;
}
