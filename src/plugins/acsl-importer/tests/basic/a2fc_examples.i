/* run.config
 DEPS: includes/file2.acsl
   STDOPT: -no-unicode -acsl-import %{dep:./a2fc_examples.acsl} -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */

/* -------------------- */

// ###### 1.1.7 <acsl_volatile_clause>

/*@ requires to_keep_the_unused_function: \true; */
extern int rd_z(int volatile *p) ;
volatile int z;

// ###### 2.1 <funspec_spec>
extern void exit(int val);

void may_exit(int cond, int val) {
  if (cond) exit (val);
}

// ###### 2.1 <funspec_spec>
//    and 2.2.1 <acsl_loop_annotation>
int extended_Euclid(int x, int y, int *p, int *q) {
  int a = 1, b = 0, c = 0, d = 1;
  while (y > 0) {
    int r = x % y;
    int q = x / y;
    int ta = a, tb = b;
    x = y; y = r;
    a = c; b = d;
    c = ta - c * q; d = tb - d * q;
  }
  *p = a; *q = b;
  return x;
}

// ###### 2.2.3 <acsl_stmt_pec>
int abrupt_termination(int x) {  
  while (x > 0) {  
body: {
      if (x % 11 == 0) break;
      x--;
      if (x % 7 == 0) continue;
      x--;
      if (x % 5 == 0) return x;
      x--;
    }
  }
  return x;
}

/* -------------------- */
int asm_call(int x) {
  int five_times;

  asm ("leal (%1,%1,4), %0"
       : "=r" (five_times)
       : "r" (x)
       );
  return five_times;
}

/* -------------------- */
void (*ptr) (int *) ;
int X;
int indirect_call(int x) {
  (*ptr) (&x);
  (*ptr) (&x);
  (*ptr) (&x);
  return asm_call (asm_call (x));
}

/* -------------------- */

extern void main (void);
