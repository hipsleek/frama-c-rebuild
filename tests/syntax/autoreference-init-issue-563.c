/* run.config
   STDOPT: #"-cpp-extra-args=-DCORRECTINIT"
   EXIT: 3
   STDOPT: #"-cpp-extra-args=-DAUTOINIT"
   STDOPT: #"-cpp-extra-args=-DADDREFFECTS"
*/

/* This file test several forms of autoreferences in initializations.
   Autoreferences are technically allowed and useful in some cases, but it can
   lead to undefined behaviors. As of now it is difficult to handle certain
   cases in Frama-C, which rarely occur in real code, so we reject these codes
   for now.
*/

#include <stdlib.h>

struct s {
  int a;
  int b[];
};

struct _cell {
  int value;
  struct _cell *prev;
  struct _cell *next;
};

void f() {

  #ifdef CORRECTINIT
  int a[5]={ a[2], 42, a[3] };
  struct _cell arr_1[2] = {
      { 0, &arr_1[1], &arr_1[1] },
      { 1, &arr_1[0], &arr_1[0] }
    };
  // This will be collapsed so we can keep the mention of x1.
  struct s *x1 = malloc(sizeof(*x1) + sizeof(int)*10);
  // Here the normalization will transform the expression into a block, so
  // collapse won't happen: keep the tmp variable.
  struct s *x2 = (a[0] = 1, malloc(sizeof(*x2) + sizeof(int)*10));
  size_t x3 = sizeof(x3) > 6 ? sizeof(x3): 6;
  #endif

  #ifdef AUTOINIT
  // Unsupported by Frama-C
  //
  // The side-effect to assign 'b' needs to be done outside the initialization
  // because Frama-C's internal AST does not allow side-effects in expressions.
  // Ideally we would like to declare 'b' and then do a undefined sequence
  // between the assignment and the initialization, but Frama-C does not allow
  // to do that trivially.
  int b[4]={ b[2], 42, b[3] = 1 };
  #endif

  #ifdef ADDREFFECTS
  // Unsupported by Frama-C.
  //
  // The address of 'arr_2' needs to be available before its initialization for
  // the same reasons than the example above. The suggested solution would also
  // fix this case.
  struct _cell *y;
  struct _cell arr_2[2] = {
      { 0, (y = &arr_2[1], &arr_2[1]), &arr_2[1] },
      { 1, &arr_2[0], &arr_2[0] }
    };
  #endif
}
