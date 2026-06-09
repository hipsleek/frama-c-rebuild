/* run.config*
STDOPT: +"-machdep gcc_x86_32"
*/
#include <stddef.h>
unsigned int S;
unsigned int A,B;
#define SIZE 4
#define TESTa(c, s, a) S=s, A=a
#define TESTb(c, s, a, b) S=s,A=a,B=b

//--------------------------------------------------------------------
struct c {
  char ca;
};

static void ct(void) {
  TESTa("c", sizeof(struct c), offsetof(struct c, ca));
  //         : size :  01
  //     gcc :   1  : |a|
}

//--------------------------------------------------------------------
struct d {
  char da;
} __attribute__((__aligned__(SIZE)));

static void dt(void) {
  TESTa("d", sizeof(struct d), offsetof(struct d, da));
  //         : size :  01234
  //     gcc :   4  : |a---|
}

//--------------------------------------------------------------------
struct p {
  char pa __attribute__((__aligned__(SIZE)));
};

static void pt(void) {
  TESTa("p", sizeof(struct p), offsetof(struct p, pa));
  //         : size :  01234
  //     gcc :   4  : |a---|
}

//--------------------------------------------------------------------
struct q {
  char qa __attribute__((__aligned__(SIZE)));
  char qb;
};

static void qt(void) {
  TESTb("q", sizeof(struct q), offsetof(struct q, qa), offsetof(struct q, qb));
  //         : size :  01234
  //     gcc :   4  : |ab--|
}

//--------------------------------------------------------------------
struct r {
  char ra;
  char rb __attribute__((__aligned__(SIZE)));
};

static void rt(void) {
  TESTb("r", sizeof(struct r), offsetof(struct r, ra), offsetof(struct r, rb));
  //         : size :  012345678
  //     gcc :   8  : |a---b---|
}

//--------------------------------------------------------------------
struct s {
  char sa __attribute__((__aligned__(SIZE)));
  char sb __attribute__((__aligned__(SIZE)));
};

static void st(void) {
  TESTb("s", sizeof(struct s), offsetof(struct s, sa), offsetof(struct s, sb));
  //         : size :  012345678
  //     gcc :   8  : |a---b---|
}

//--------------------------------------------------------------------
struct t {
  char ta;
  char tb[0] __attribute__((__aligned__(SIZE)));
};

static void tt(void) {
  TESTb("t", sizeof(struct t), offsetof(struct t, ta), offsetof(struct t, tb));
  //         : size :  012345678 : comment
  //     gcc :   4  : |a---|     : b at offset 4, outside the struct
  // frama-c :   8  : |a---b---| : b of size 1 instead of 0
}

//--------------------------------------------------------------------
typedef float float_aligned16 __attribute__((aligned(16))); // increase
typedef double double_aligned1 __attribute__((aligned(1))); // reduce

static void typedef_with_aligned(void) {
  int a;
  a = __alignof__(float_aligned16);
  //@ check a == 16;
  a = __alignof__(double_aligned1);
  //@ check a == 1;
}

//--------------------------------------------------------------------

/* Test the aligned attribute applied either on struct type or on variables. */

#include <stdalign.h>

/* Aligned attribute on the struct type: final padding bits are added to match
   the struct alignment, so the size of the struct is 4 bytes. */

struct s4 { char c; } __attribute__((aligned(4)));
struct s4 struct_aligned1;

typedef struct { char c; } __attribute__((aligned(4))) ts4;
ts4 struct_aligned2;

struct { char c; } __attribute__((aligned(4))) struct_aligned3;

/* Aligned attribute on the variable: only impact the alignment of the variable,
   but its type is unchanged and its size is 1 byte. */

struct s1 { char c; };
struct s1 __attribute__((aligned(4))) var_aligned1;

typedef struct { char c; } ts1;
ts1 __attribute__((aligned(4))) var_aligned2;

struct { char c; } (__attribute__((aligned(4))) var_aligned3);

void struct_aligned (void) {
  struct_aligned1.c = 1;
  struct_aligned2.c = 2;
  struct_aligned3.c = 3;
  var_aligned1.c = 1;
  var_aligned2.c = 2;
  var_aligned3.c = 3;

  /* Size of aligned structs must be 4. */
  Frama_C_show_each_4(sizeof(struct_aligned1), sizeof(struct_aligned2),
                      sizeof(struct_aligned3));

  /* Size of aligned variables must be 1. */
  Frama_C_show_each_1(sizeof(var_aligned1), sizeof(var_aligned2),
                      sizeof(var_aligned3));

  /* All alignments must be 4. */
  Frama_C_show_each_4(alignof(struct_aligned1), alignof(struct_aligned2),
                      alignof(struct_aligned3), alignof(var_aligned1),
                      alignof(var_aligned1), alignof(var_aligned1));

  /* No alarm as all alignments are 4. */
  int *ptr4;
  ptr4 = (int *)&struct_aligned1.c;
  ptr4 = (int *)&struct_aligned2.c;
  ptr4 = (int *)&struct_aligned3.c;
  ptr4 = (int *)&var_aligned1.c;
  ptr4 = (int *)&var_aligned1.c;
  ptr4 = (int *)&var_aligned1.c;

  /* Alignment alarms at each line, as all alignments are 4. */
  int __attribute((aligned(8))) *ptr8;
  ptr8 = (long *)&struct_aligned1.c;
  ptr8 = (long *)&struct_aligned2.c;
  ptr8 = (long *)&struct_aligned3.c;
  ptr8 = (long *)&var_aligned1.c;
  ptr8 = (long *)&var_aligned1.c;
  ptr8 = (long *)&var_aligned1.c;
}

//--------------------------------------------------------------------


int main(void)
{
  ct();
  dt();
  pt();
  qt();
  rt();
  st();
  tt();
  typedef_with_aligned();
  struct_aligned();
  return 0;
}
