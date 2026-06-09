/* run.config
 EXIT: 0
   STDOPT:
 EXIT: 1
   STDOPT: #"-cpp-extra-args='-DINCOMPATIBLE1'"
   STDOPT: #"-cpp-extra-args='-DINCOMPATIBLE2'"
   STDOPT: #"-cpp-extra-args='-DINCOMPATIBLE3'"
   STDOPT: #"-cpp-extra-args='-DINCOMPATIBLE4'"
   STDOPT: #"-cpp-extra-args='-DINCOMPATIBLE5'"
   STDOPT: #"-cpp-extra-args='-DINCOMPATIBLE6'"
   STDOPT: #"-cpp-extra-args='-DINCOMPATIBLE7'"
   STDOPT: #"-cpp-extra-args='-DINCOMPATIBLE8'"
 */
// Note: some of the declarations below are accepted by GCC 7.1.1 with -std=c11
// (but not -std=c99), mainly due to this warning (use -Wextra to see it):
// warning: type qualifiers ignored on function return type

int f(int a, char volatile *b);
#ifdef INCOMPATIBLE1
int f(int a, char *b); // conflicting types for 'f'
#endif

int g(int volatile a, char b);
int g(int a, char b); // allowed

void h(const char[]);
#ifdef INCOMPATIBLE2
void h(char[]); // conflicting types for 'h'
#endif

void i(int *f(int));
void i(int *const f(volatile int)); // allowed

void j(int *f(int[]));
#ifdef INCOMPATIBLE3
void j(int *f(int const[])); // conflicting types for 'j'
#endif

void k(int *const f(int));
const void k(int *f(volatile int)); // accepted by GCC 7.1.1 with -std=c11

typedef volatile int(*fp1)(char);
typedef int(*fp2)(const char);

fp1 *l(int *f(int));
#ifdef INCOMPATIBLE4
fp2 *l(const int *f(volatile int)); // conflicting types for 'l'
#endif

fp1 **const m(int *const f(int));
fp2 **m(int *f(volatile int)); // accepted by GCC 7.1.1 with -std=c11

fp1 *const *n(int *f(int, fp1 *[]));
#ifdef INCOMPATIBLE5
fp2 *const *n(int *f(volatile int, fp2 *const[])); // conflicting types for 'n'
#endif

void o(char r1, char r2);
#ifdef INCOMPATIBLE6
void o(restrict char r1, restrict char r2);
#endif

void p(const int a);
void p(int a);
void p(volatile int a);

typedef int* iptr;

void q(const iptr p);
void q(volatile iptr p); // allowed

void r(fp1 f, fp1 g);
#ifdef INCOMPATIBLE7
void r(restrict fp1 f, restrict fp2 g);
#endif

typedef int *restrict irptr;
void s(irptr p1, irptr p2);
#ifdef INCOMPATIBLE8
void s(restrict irptr p1, restrict irptr p2);
typedef int restrict* riptr;
int restrict rga[1];
int restrict *rgp;
int restrict rgi;
typedef int restrict (*fp3)(const char);
typedef int (*restrict fp4)(const char);
#endif
int *restrict *restrict iprpr;
int *restrict matrix[1];

int main() {
  f(0, 0);
  g(0, 0);
  h(0);
  i(0);
  j(0);
  k(0);
  l(0);
  m(0);
  n(0);
  o(0, 0);
  p(0);
  q(0);
  r(0, 0);
  s(0, 0);
  return 0;
}
