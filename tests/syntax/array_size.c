/* run.config*
 EXIT: 1
   OPT: -cpp-extra-args="-DNEG_SIZE"
   OPT: -cpp-extra-args="-DVLA_INIT1"
   OPT: -cpp-extra-args="-DVLA_INIT2"
   OPT: -cpp-extra-args="-DVLA_GLOB1"
   OPT: -cpp-extra-args="-DVLA_GLOB2"
   OPT: -cpp-extra-args="-DVLA_STRUCT"
*/

#ifdef NEG_SIZE
int f(int t[-1]) { return t[0]; } // should raise an error
#endif

#ifdef VLA_INIT1
void g(int size) {
  int a[size] = { 0 }; // error: VLA can't be initialized
}
#endif

#ifdef VLA_INIT2
void h (int size) {
  int a [size][2] = { { 0 } }; // same as above
}
#endif

const int size = 42;

#ifdef VLA_GLOB1
int a[size]; // error: global arrays must have a fixed size.
#endif

#ifdef VLA_GLOB2
int a[2][size]; // same as above
#endif

#ifdef VLA_STRUCT
struct { int a [size]; }; // error: no VLA in struct
#endif
