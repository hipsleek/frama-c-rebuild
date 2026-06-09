/* run.config
   MODULE: @PTEST_NAME@
   STDOPT:
   STDOPT: #"-cpp-extra-args=-DALIGNAS_EXTENDED" +"-machdep gcc_x86_32"
   STDOPT: #"%{dep:./alignas_typing_constrains_companion.c} -cpp-extra-args=-DALIGNAS_IN_STRUCT_IN_OTHER_TU_COMPATIBLE"
   STDOPT: #"%{dep:./alignas_typing_constrains_companion.c} -cpp-extra-args=-DALIGNAS_IN_UNION_IN_OTHER_TU_COMPATIBLE"
   EXIT: 1
   STDOPT: #"-cpp-extra-args=-DALIGNAS_WEAKER_1"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_WEAKER_2"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_WEAKER_3"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_WEAKER_4"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_WEAKER_5"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_EXTENDED"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_EXTENDED_TOO_BIG" +"-machdep gcc_x86_32"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_OCAML_TOO_BIG"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_NOT_VALID_NEG"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_NOT_VALID_NOT_POW2"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_NOT_CONSTANT"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_TYPEDEF_1"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_TYPEDEF_2"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_TYPEONLY"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_BITFIELD"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_FUNCTION_1"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_FUNCTION_2"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_FUNCTION_3"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_FUNCTION_4"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_FORMAL"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_REGISTER"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_COMBINE_REDUCE"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_DIFFERENT_IN_ORIGINAL_DEF"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_NOT_IN_ORIGINAL_DEF"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_DIFFERENT_IN_DECLS"
   STDOPT: #"%{dep:./alignas_typing_constrains_companion.c} -cpp-extra-args=-DALIGNAS_IN_STRUCT_IN_OTHER_TU_INCOMPATIBLE"
   STDOPT: #"%{dep:./alignas_typing_constrains_companion.c} -cpp-extra-args=-DALIGNAS_IN_UNION_IN_OTHER_TU_INCOMPATIBLE"
   STDOPT: #"-cpp-extra-args=-DALIGNAS_SELF_COMPOUND"
*/

/* All these tests are done assuming x86_32 */

#include <stddef.h>

struct Al1
{
  char c;
};

struct Al4
{
  long int c;
};

// int _Alignas(8) al_loc1 ;
// _Alignas(8) int al_loc2 ;

void c17_6_2_8_1(void) // _Alignas cannot specify a weaker alignment
// + c17_6_2_8_2       // _Alignas(n) with n <= _Alignof(max_align_t) always allowed
{
  // Note: on our test architecture long int == long long int
  // Note: on our test architecture alignof(double) == 4

  _Static_assert(_Alignof(max_align_t) == 16);

  char _Alignas(1) c1;
  char _Alignas(2) c2;
  char _Alignas(4) c3;
  char _Alignas(8) c4;
  char _Alignas(16) c5;

  short _Alignas(2) s2;
  short _Alignas(4) s3;
  short _Alignas(8) s4;
  short _Alignas(16) s5;

  int _Alignas(4) i3;
  int _Alignas(8) i4;
  int _Alignas(16) i5;

  long int _Alignas(4) li3;
  long int _Alignas(8) li4;
  long int _Alignas(16) li5;

  float _Alignas(4) f2;
  float _Alignas(8) f3;
  float _Alignas(16) f4;

  double _Alignas(4) d2;
  double _Alignas(8) d3;
  double _Alignas(16) d4;

  struct Al1 _Alignas(1) al1_1;
  struct Al1 _Alignas(4) al1_2;
  struct Al1 _Alignas(8) al1_3;
  struct Al1 _Alignas(16) al1_4;

  struct Al4 _Alignas(4) al4_2;
  struct Al4 _Alignas(8) al4_3;
  struct Al4 _Alignas(16) al4_4;

#ifdef ALIGNAS_WEAKER_1
  short _Alignas(1) s1;
#endif
#ifdef ALIGNAS_WEAKER_2
  int _Alignas(2) i2;
#endif
#ifdef ALIGNAS_WEAKER_3
  float _Alignas(2) f1;
#endif
#ifdef ALIGNAS_WEAKER_4
  double _Alignas(2) d1;
#endif
#ifdef ALIGNAS_WEAKER_5
  struct Al4 _Alignas(2) al8_1;
#endif
}

#ifdef ALIGNAS_EXTENDED
void c17_6_2_8_3(void)
// _Alignas(n) with n > _Alignof(max_align_t) might be allowed by the implementation
{
  // on our architecture:
  _Static_assert(_Alignof(max_align_t) == 16);

  char _Alignas(32) x;
}
#endif

#ifdef ALIGNAS_EXTENDED_TOO_BIG
void c17_6_2_8_3(void)
// _Alignas(n) with n > _Alignof(max_align_t) might be allowed by the implementation
{
  // on our architecture:
  _Static_assert(_Alignof(max_align_t) == 16);

  char _Alignas(1 << 29) x; /* GCC limits to 1 << 28 */
}
#endif

#ifdef ALIGNAS_OCAML_TOO_BIG
void c17_6_2_8_3(void)
// Frama-C cannot handle that (OCaml limitation)
{
  char _Alignas(1ull << 63) x;
}
#endif

void c17_6_2_8_4(void)
// a valid alignment is 0 or a power of two
{
  size_t no_cast = _Alignof(int);

  char _Alignas(0) c0;
  char _Alignas(1) c1;
  char _Alignas(2) c2;
  char _Alignas(4) c3;
  char _Alignas(8) c4;
  char _Alignas(16) c5;

#ifdef ALIGNAS_NOT_VALID_NEG
  char _Alignas(-1) c_negative;
#endif
#ifdef ALIGNAS_NOT_VALID_NOT_POW2
  char _Alignas(3) c_not_power_2;
#endif
}

// c17_6_2_8_5: an address that satisfies an alignment satisfies the weaker ones
// --> not a typing property

// c17_6_2_8_6: see alignof_typing_constrains.c

// c17_6_2_8_7: comparing alignment is comparing their numeric values
// --> not a typing property

void c17_6_7_5_1(void)
// accepts types and constant expressions
{
  char _Alignas(4) c0;
  char _Alignas(int) c1;
#ifdef ALIGNAS_NOT_CONSTANT
  char _Alignas(c0) c1;
#endif
}

// c17 6.7.5.2
// No _Alignas for typedefs, bitfields, functions, parameters or register

#ifdef ALIGNAS_TYPEDEF_1
typedef int _Alignas(8) c17_6_7_5_2_a;
#endif

#ifdef ALIGNAS_TYPEDEF_2
struct c17_6_7_5_2_a _Alignas(8);
#endif

#ifdef ALIGNAS_TYPEONLY
int g = sizeof(struct only _Alignas(8));
#endif

#ifdef ALIGNAS_BITFIELD
struct c17_6_7_5_2_b
{
  unsigned _Alignas(8) b : 4;
};
#endif

#ifdef ALIGNAS_FUNCTION_1
// note: no smart message, we just check that it is not captured as an attribute
void c17_6_7_5_2_c(int x) _Alignas(8)
{
}
#endif

#ifdef ALIGNAS_FUNCTION_2
_Alignas(8) void c17_6_7_5_2_c(int x)
{
}
#endif

#ifdef ALIGNAS_FUNCTION_3
// note: no smart message, we just check that it is not captured as an attribute
void c17_6_7_5_2_c(int x) _Alignas(8) ;
#endif

#ifdef ALIGNAS_FUNCTION_4
_Alignas(8) void c17_6_7_5_2_c(int x) ;
#endif


#ifdef ALIGNAS_FORMAL
void c17_6_7_5_2_d(int _Alignas(8) formal)
{
}
#endif

#ifdef ALIGNAS_REGISTER
void c17_6_7_5_2_e(void){
  register int _Alignas(8) v ;
}
#endif

// c17_6_7_5_3: _Alignas accepts valid or extended alignment.
// --> cf c17_6_2_8_3/4 above

// c17_6_7_5_4: no implementation for testing that

// c17_6_7_5_5:
#ifdef ALIGNAS_COMBINE_REDUCE
typedef struct A { _Alignas(8) char c; } A;
typedef struct B { _Alignas(1) A a; } B;
#endif

// c17_6_7_5_6: _Alignas(typ) == _Alignas(_Alignof(typ))
// --> by definition during parsing, so that the following declaration should
//     be compatible:
int _Alignas(long) alignas_typ = 0 ;
extern int _Alignas(_Alignof(long)) alignas_typ ;

void c17_6_7_5_7(void)
// when _Alignas(0) --> no effect
// when multiple _Alignas the strictest wins
{
  int _Alignas(0) zero ;
  int _Alignas(8) _Alignas(16) several_1 ;
  int _Alignas(16) _Alignas(8) several_2 ;
}

int _Alignas(8) i = 0 ;
int j = 0;

void c17_6_7_5_8(void){
  extern int _Alignas(8) i;
  extern int i;

  extern int _Alignas(8) first_decl_has ;
  extern int first_decl_has ;

  extern int first_decl_has_not ;
  extern int _Alignas(8) first_decl_has_not ;

  extern int _Alignas(8) same ;
  extern int _Alignas(8) same ;

#ifdef ALIGNAS_DIFFERENT_IN_ORIGINAL_DEF
  extern int _Alignas(4) i;
#endif

#ifdef ALIGNAS_NOT_IN_ORIGINAL_DEF
  extern int _Alignas(4) j;
#endif

#ifdef ALIGNAS_DIFFERENT_IN_DECLS
  extern int _Alignas(4) k;
  extern int _Alignas(8) k;
#endif
}

// see alignas_typing_constrains_companion.c
// c17_6_2_7_1

#ifdef ALIGNAS_IN_STRUCT_IN_OTHER_TU_COMPATIBLE
struct SAlF {
  int _Alignas(8) field ;
};
struct SAlF get_SAlF(void);
void use(void){
  struct SAlF s1 = get_SAlF() ;
}
#endif

#ifdef ALIGNAS_IN_STRUCT_IN_OTHER_TU_INCOMPATIBLE
struct SAlF {
  int _Alignas(16) field ;
};
struct SAlF get_SAlF(void);
void use(void){
  struct SAlF s1 = get_SAlF() ;
}
#endif

#ifdef ALIGNAS_IN_UNION_IN_OTHER_TU_COMPATIBLE
union UAlF {
  int _Alignas(8) field ;
};
union UAlF get_UAlF(void);
void use(void){
  union UAlF s1 = get_UAlF() ;
}
#endif

#ifdef ALIGNAS_IN_UNION_IN_OTHER_TU_INCOMPATIBLE
union UAlF {
  int _Alignas(16) field ;
};
union UAlF get_UAlF(void);
void use(void){
  union UAlF s1 = get_UAlF() ;
}
#endif

#ifdef ALIGNAS_SELF_COMPOUND
struct X { // should be refused: _Alignof on incomplete type
  int _Alignas(struct X) x ;
} ;
#endif
