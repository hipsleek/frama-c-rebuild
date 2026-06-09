/* run.config*
   STDOPT: +"-machdep x86_64 -warn-unaligned-pointer"
*/

/* Tests:
   - the emission of alignment alarms
   - the evaluation of ACSL predicate \aligned
*/

#include <stdint.h>

volatile unsigned int nondet;

char c;  // alignof = 1
short s; // alignof = 2
int i;   // alignof = 4
long l;  // alignof = 8

_Alignas(2) char c2;
_Alignas(4) char c4;
_Alignas(8) int i8;

char *c_ptr;
short *s_ptr;
int *i_ptr;
long *l_ptr;

/* ----------------------------------------------------------------------------
            Alignment alarms on conversions from integers to pointers
  --------------------------------------------------------------------------- */

void int_constant_to_ptr (void){
  c_ptr = (char *) 1;
  s_ptr = (short *) 2;
  i_ptr = (int *) 4;
  l_ptr = (int *) 8;

  // Completely invalid alignment: invalid alarms.

  if (nondet) { s_ptr = (short *) 1; }
  if (nondet) { i_ptr = (int *) 2; }
  if (nondet) { l_ptr = (long *) 4; }
}

void int_to_ptr_aux (unsigned int limit){

  unsigned int any = nondet % limit;
  unsigned int any_mod2 = 2 * any;
  unsigned int any_mod4 = 2 * any_mod2;
  unsigned int any_mod8 = 2 * any_mod4;

  /* Valid alignment. */

  c_ptr = (char *) any;
  c_ptr = (char *) any_mod2;
  c_ptr = (char *) any_mod4;
  c_ptr = (char *) any_mod8;

  s_ptr = (short *) any_mod2;
  s_ptr = (short *) any_mod4;
  s_ptr = (short *) any_mod8;

  i_ptr = (int *) any_mod4;
  i_ptr = (int *) any_mod8;

  l_ptr = (long *) any_mod8;

  /* In statements below, alignment may be invalid: Eva should emit an unknown
     alarm and reduce the possible integer values such that no alarm is emitted
     on the same statement afterward. */

  s_ptr = (short *) any;
  s_ptr = (short *) any; // No alarm if [any] has been reduced.
  i_ptr = (int *) any;
  i_ptr = (int *) any; // No alarm if [any] has been reduced.
  i_ptr = (int *) any_mod2;
  i_ptr = (int *) any_mod2; // No alarm if [any_mod2] has been reduced.
  l_ptr = (long *) any;
  l_ptr = (long *) any; // No alarm if [any] has been reduced.
  l_ptr = (long *) any_mod2;
  l_ptr = (long *) any_mod2; // No alarm if [any_mod2] has been reduced.
  l_ptr = (long *) any_mod4;
  l_ptr = (long *) any_mod4; // No alarm if [any_mod4] has been reduced.
}

void int_to_ptr (void) {
  int_to_ptr_aux(5); // small limit to test small integer sets.
  int_to_ptr_aux(100); // higher limit to test intervals.
}

/* Tests that alignment does not change with conversions from pointer to
   integer.*/
void ptr_to_int (void) {
  int *p, *p2;
  uintptr_t addr;
  p = nondet ? &i : (int *) &l;
  addr = (uintptr_t) p;
  p2 = (int *) addr; // No alarm.
  p = nondet ? &i : (int *) &s; // Alignment alarm
  addr = (uintptr_t) p;
  p2 = (int *) addr; // Alignment alarm.
}

/* ----------------------------------------------------------------------------
              Alignment alarms on conversions between pointers
  --------------------------------------------------------------------------- */

void addrof_to_ptr (void) {

  // All pointers below have a valid alignment: no alarm should be emitted.

  c_ptr = &c;
  if (nondet) c_ptr = &s;
  if (nondet) c_ptr = &i;
  if (nondet) c_ptr = &l;
  if (nondet) c_ptr = &c2;
  if (nondet) c_ptr = &c4;
  if (nondet) c_ptr = &i8;

  s_ptr = &s;
  if (nondet) s_ptr = &i;
  if (nondet) s_ptr = &l;
  if (nondet) s_ptr = &c2;
  if (nondet) s_ptr = &c4;
  if (nondet) s_ptr = &i8;

  i_ptr = &i;
  if (nondet) i_ptr = &l;
  if (nondet) i_ptr = &c4;
  if (nondet) i_ptr = &i8;

  l_ptr = &l;
  if (nondet) l_ptr = &i8;

  if (nondet) {
    c_ptr = s_ptr;
    s_ptr = i_ptr;
    i_ptr = l_ptr;
  }

  /* All pointer conversion below may create a pointer with invalid alignment:
     unknown alarms should be emitted. */

  s_ptr = &c;
  i_ptr = &c;
  i_ptr = &c2;
  i_ptr = &s;
  l_ptr = &c;
  l_ptr = &c2;
  l_ptr = &s;
  l_ptr = &i;
  l_ptr = &c4;

  l_ptr = i_ptr;
  i_ptr = s_ptr;
  s_ptr = c_ptr;
}

_Alignas(4) char t[200];

void alignment_in_array (void) {
  int set_offset = nondet % 5; // offset represented as a concrete set
  int itv_offset = nondet % 20; // offset represented as an interval

  // All pointers below are aligned: no alarm.

  c_ptr = (char *) &t[1];
  s_ptr = (short *) &t[6];
  i_ptr = (int *) &t[12];

  c_ptr = (char *) &t[set_offset];
  s_ptr = (short *) &t[2 * set_offset];
  i_ptr = (int *) &t[4 * set_offset];

  c_ptr = (char *) &t[itv_offset];
  s_ptr = (short *) &t[2 * itv_offset];
  i_ptr = (int *) &t[4 * itv_offset];

  // All pointers below may be unaligned.

  if (nondet) s_ptr = (short *) &t[1]; // Invalid alarm
  if (nondet) i_ptr = (int *) &t[2]; // Invalid alarm

  if (nondet) l_ptr = (long *) &t[1]; // Invalid alarm.
  l_ptr = (long *) &t[4]; // Unknown alarm: the base could be aligned.
  l_ptr = (long *) &t[8]; // Unknown alarm: the base could be aligned.

  /* Each line below must produce an unknown alarm, as some offsets are valid
     and some other invalid. */

  if (nondet) s_ptr = (short *) &t[set_offset];
  if (nondet) i_ptr = (int *) &t[2 * set_offset];
  if (nondet) l_ptr = (long *) &t[8 * set_offset];

  if (nondet) s_ptr = (short *) &t[itv_offset];
  if (nondet) i_ptr = (int *) &t[2 * itv_offset];
  if (nondet) l_ptr = (long *) &t[8 * itv_offset];
}

struct S {
  char c0;
  char c1;
  char c2;
  char c3;
  char c4;
  char c5;
  // two bytes of padding
  int i0;
  int i1;
  _Alignas(4) short s4;
} st;

void alignment_in_struct (void) {

  // All pointers below are aligned: no alarm.
  s_ptr = (short *) &st.c2;
  s_ptr = (short *) &st.i0;
  s_ptr = (short *) &st.s4;
  i_ptr = (int *) &st.c4;
  i_ptr = (int *) &st.i1;
  i_ptr = (int *) &st.s4;

  // All pointers below are unaligned: invalid alarms.
  if (nondet) s_ptr = (short *) &st.c1;
  if (nondet) i_ptr = (int *) &st.c1;
  if (nondet) i_ptr = (int *) &st.c2;
  if (nondet) l_ptr = (long *) &st.c3;

  // All pointers below may be unaligned: unknown alarms.
  l_ptr = (long *) &st;
  l_ptr = (long *) &st.i0;
  l_ptr = (long *) &st.s4;
}

struct Packed {
  char c;
  short s;
  int i;
} __attribute__ ((__packed__)) st_packed;

void alignment_in_packed_struct (void) {

  c_ptr = &st_packed; // Aligned pointer: no alarm

  /* [c_ptr] is aligned but &st_packed.s is an unaligned short pointer,
     before its conversion to char*. */
  c_ptr = (char *) &st_packed.s;
  c_ptr = (char *) &st_packed.i;

  // All pointers below may be unaligned: unknown alarms.
  s_ptr = (short *) &st_packed;
  i_ptr = (int *) &st_packed;
  s_ptr = &st_packed.s;
  i_ptr = &st_packed.i;
  s_ptr = (short *) &st_packed.i;
}

/* ----------------------------------------------------------------------------
           Alignment alarms after untyped write of pointer values
  --------------------------------------------------------------------------- */

union IPtr {
  int *ptr;
  uintptr_t i;
};

/* Tests the creation of unaligned pointers without explicit pointer conversion
   through untyped write.*/
void unaligned_pointer_creation (void) {
  char c[4] = {0};
  int i;
  int *p, *q;
  *((char **)&p) = &c;
  // In all statements below, p may be unaligned!
  q = p;
  i = *p;
  *p = 0;

  union IPtr u;
  u.i = (uintptr_t)&c;
  // In all statements below, u.ptr may be unaligned!
  q = u.ptr;
  i = *u.ptr;
  *u.ptr = 0;
}

/* ----------------------------------------------------------------------------
                            ACSL predicate \aligned
  --------------------------------------------------------------------------- */

/* Tests the evaluation of \aligned. */
void aligned_predicate (void) {
  char *ptr8, *ptr4, *ptr2, *ptr1;

  ptr1 = &c;
  ptr2 = nondet ? (char *) &s : &c2;
  ptr4 = nondet ? (char *) &i : &c4;
  ptr8 = nondet ? (char *) &l : (char *) &i8;

  if (nondet) ptr4 = ptr8;
  if (nondet) ptr2 = ptr4;
  if (nondet) ptr1 = ptr2;

  //@ check true: \aligned(ptr8, 8);
  //@ check true: \aligned(ptr8, alignof(long));
  //@ check true: \aligned(ptr4, 4);
  //@ check true: \aligned(ptr4, alignof(int));
  //@ check true: \aligned(ptr2, 2);
  //@ check true: \aligned(ptr2, alignof(short));
  //@ check true: \aligned(ptr1, 1);
  //@ check true: \aligned(ptr1, alignof(char));

  //@ check true: \aligned(ptr8, 4);
  //@ check true: \aligned(ptr8, alignof(int));
  //@ check true: \aligned(ptr4, 2);
  //@ check true: \aligned(ptr4, alignof(short));
  //@ check true: \aligned(ptr2, 1);
  //@ check true: \aligned(ptr2, alignof(char));

  //@ check false: \aligned(ptr8, 3);
  //@ check false: \aligned(ptr8, 9);
  //@ check false: \aligned(ptr8, 16);
  //@ check false: \aligned(ptr4, 8);
  //@ check false: \aligned(ptr4, alignof(long));
  //@ check false: \aligned(ptr2, 4);
  //@ check false: \aligned(ptr2, alignof(int));
  //@ check false: \aligned(ptr1, 2);
  //@ check false: \aligned(ptr1, alignof(short));

  //@ check unknown: \aligned(ptr1, 0);

  int x = nondet % 10;
  //@ check unknown: \aligned(ptr2, x);
}

/* ----------------------------------------------------------------------------
                                    Main
  --------------------------------------------------------------------------- */

void main (void) {
  int_constant_to_ptr();
  int_to_ptr();
  ptr_to_int();
  addrof_to_ptr();
  alignment_in_array();
  alignment_in_struct();
  alignment_in_packed_struct();
  unaligned_pointer_creation();
  aligned_predicate();
}
