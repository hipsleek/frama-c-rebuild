/* run.config
   EXIT: 1
   STDOPT: #"-cpp-extra-args=-DNONE"
   STDOPT: #"-cpp-extra-args=-DTOO_MANY_DEFAULTS"
   STDOPT: #"-cpp-extra-args=-DTOO_MANY_COMPATIBLE"
   STDOPT: #"-cpp-extra-args=-DTOO_MANY_COMPATIBLE2"
   STDOPT: #"-cpp-extra-args=-DTOO_MANY_COMPATIBLE3"
   STDOPT: #"-cpp-extra-args=-DINCOMPLETE_TYPE"
   STDOPT: #"-cpp-extra-args=-DINCOMPATIBLE_QUALIFIED_TYPE"
   STDOPT: #"-cpp-extra-args=-DFUNCTION_TYPE"
   STDOPT: #"-cpp-extra-args=-DVLA"
   EXIT: 0
   STDOPT:
*/

// Some tests inspired by llvm/clang/test/Sema/generic-selection.c

typedef unsigned int my_uint;
typedef int (*fptr)(int);
typedef void (*vfptr)(int);
int foo(int i) { return 0; }
void void_foo(int i) {}

// Example from the C11 standard
double cbrt(double x);
float cbrtf(float x);
long double cbrtl(long double x);
#define cbrt(X) _Generic((X),                   \
                         long double: cbrtl,    \
                         default: cbrt,         \
                         float: cbrtf           \
                         )(X)

int main() {
#ifdef NONE
  int a = _Generic("abc");
#endif
#ifdef TOO_MANY_DEFAULTS
  int a = _Generic("abc", default: 1, default: 1);
#endif
#ifdef TOO_MANY_COMPATIBLE
  // compatibility via typedef
  int a = _Generic(42, my_uint: 1, unsigned int: 2);
#endif
#ifdef TOO_MANY_COMPATIBLE2
  // compatibility modulo implicit arguments
  int a = _Generic(0,
      void (*)():     0,
      void (*)(void): 0);
#endif
#ifdef TOO_MANY_COMPATIBLE3
  // implicit arguments compatible between first and second selector,
  // but the selectors themselves are not compatible between them
  int a = _Generic((void (*)()) 0,
                   void (*)(int):  0,
                   void (*)(void): 0);
#endif
#ifdef INCOMPLETE_TYPE
  int a = _Generic(42, void: 1, default: 2);
#endif
#ifdef FUNCTION_TYPE
  int a = _Generic(1, void(int): 1);
#endif
#ifdef INCOMPATIBLE_QUALIFIED_TYPE
  int a = _Generic("abc", char const *: 0);
#endif
#ifdef VLA
  int x = 42;
  int y[x];
  int a = _Generic(y, int[x]: 0, default: 1);
#endif
  int ok1 = _Generic("abc", char*: 0);
  int ok2 = _Generic(1.0, float: 1, double: 0);
  int ok3 = _Generic(1L, short: 1, unsigned int: 2, int: 3, long: 0, unsigned long: 5);
  void p(int);
  int ok4 = _Generic(p, void(*)(int): 0, void(*)(long): 1);
  double c = cbrt(0.0f);
  int ok5 = _Generic(foo, fptr: 0, int: 4, vfptr: 5);

  char vtc[2] = {4, -3};
  double vtd[2] = {4.0, -3.0};
  double *vt = &_Generic(vt, char*: vtc[0], double*: vtd[0]);
  int x = 3;
  short y = 4;
  _Generic(42, short: y, int: x)++;
  --_Generic(42, short: y, int: x);
  _Generic(42, short: y, int: x) += 10;
  _Generic(42, short: y, int: x) = 10;
  return 0;
}
