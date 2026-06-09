/* run.config
   STDOPT: #"-warn-pointer-downcast -print"
*/

#include <inttypes.h>

// we accept pointer to integer conversions only
// if the target type is bigger, equal and unsigned,
// or is intptr_t (potentially typedef'd)

typedef intptr_t my_intptr_t;

void f() {
  int x = 0;
  int* y = &x;
  my_intptr_t z0 = (my_intptr_t) y;
  intptr_t z1 = (intptr_t)y;
  long z2 = (long)y; //should warn
  int z3 = (int) y; // should warn
  short z4 = (short) y; // should warn
}
