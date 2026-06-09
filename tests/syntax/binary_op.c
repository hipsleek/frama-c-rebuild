/* run.config
EXIT: 1
OPT:-cpp-extra-args="-DMULT"
OPT:-cpp-extra-args="-DBITWISE"
OPT:-cpp-extra-args="-DADD1"
OPT:-cpp-extra-args="-DADD2"
OPT:-cpp-extra-args="-DMIN1"
OPT:-cpp-extra-args="-DMIN2"
OPT:-cpp-extra-args="-DEQ2"
OPT:-cpp-extra-args="-DCMP1"
OPT:-cpp-extra-args="-DCMP2"
OPT:-cpp-extra-args="-DCMP3"
OPT:-cpp-extra-args="-DCMP4"
EXIT: 0
OPT:-print -machdep gcc_x86_64 -cpp-extra-args="-DADD2"
OPT:-print -machdep gcc_x86_64 -cpp-extra-args="-DMIN1"
OPT:-print -cpp-extra-args="-DEQ1"
*/

#include <stddef.h>

int* ptr;

#ifdef MULT
// MUL only works on arithmetic types
int w(void) { return 0 * w; }
#endif

#ifdef BITWISE
// BITWISE only works on integral types
int v(void) { return 0 & 0.0; }
#endif

#ifdef ADD1
// ADD requires either both arithmetic, or a pointer and an integral
int x(void) { return ptr + ptr; }
#endif

#ifdef ADD2
// Only GCC allows function pointers here
int x(void) { return x + 0; }
#endif

#ifdef MIN1
// Only GCC allows function pointers here
ptrdiff_t x(void) { return x - x; }
#endif

#ifdef MIN2
// Incompatible types int and float
ptrdiff_t x(void) { float* fptr; return ptr - fptr; }
#endif

#ifdef EQ1
// Can compare pointers to 0
int y(void) { return ptr == 0; }
#endif

#ifdef EQ2
// Either both operands have pointer type or arithmetic type, or one is a
// pointer and the other zero
int y(void) { return ptr == 42; }
#endif

#ifdef CMP1
// Cannot compare function pointers
int y(void) { return 0 <= y; }
#endif

#ifdef CMP2
// Cannot compare function pointers
int y(void) { return y > 0; }
#endif

#ifdef CMP3
// Cannot compare function pointers
int y(void) { return y > y; }
#endif

#ifdef CMP4
// Both operands should have either pointer type or arithmetic type
int y(void) { return ptr >= 0; }
#endif
