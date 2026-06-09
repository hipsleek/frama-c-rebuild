/* run.config
   COMMENT: we need Eva for the loop unroll ACSL extension
   PLUGIN: eva
   MODULE: @PTEST_NAME@
     OPT: -machdep gcc_x86_64 -then -ast-diff %{dep:ast_diff_2.c}
     OPT: -machdep gcc_x86_64 -then -ast-diff %{dep:ast_diff_2.c} -cpp-extra-args="-DADD_ENUM_TAG"
*/
int X;
int Y=3;

int f(int x) {
  if (x <= 0)
    X = 0;
  else
    X = x;
  return X;
}

/*@ requires Y > 0;
    ensures X > 0;
    assigns X;
*/
int g() {
  X = Y;
  return X;
}

/*@ requires X > 0;
    ensures X > 0;
    assigns X;
*/
int h() {
  if (Y > 0)
    X = Y;
  return Y;
}

/*@ requires \is_finite(x);
    requires \is_finite(y);
    assigns \nothing;
    ensures \true != \false;
*/
int use_logic_builtin(double x, float y);

int has_static_local(void) {
  static int x = 0;
  x++;
  return x;
}

/*@ exits \exit_status == 1; */
int decl(void);

int used_in_decl;

int decl() {
  used_in_decl++;
  return used_in_decl;
}

void (*ptr_func)(void);

extern void i(void);

void (*ptr_func)(void) = &i;

/*@ type nat = Zero | Succ(nat); */

/*@ logic nat succ(nat n) = Succ(n); */

/*@ ensures succ(Zero) == Succ(Zero); */
extern void i(void);

void local_var_use(int v, int a[][v]) {
  int x;
  int y [sizeof(x)];
}

struct s;

extern struct s s;

enum e;

struct s* use_s() { enum e* x; return &s; }

void with_goto_changed(int c) {
  if (c) goto L1;
  X++;
  L1: X++;
  L2: X++;
}

void with_goto_unchanged(int c) {
  if (c) goto L1;
  X++;
  L1: X++;
  L2: X++;
}

enum e {
  t = 1,
  u = t + 2
};

struct s { char c[t]; };

void se() {
  struct s S;
  S.c[0] = 1;
}

void with_loop_unroll_same() {
  //@ loop unroll 5;
  for (int i = 0; i < 5; i++);
}

void with_loop_unroll_diff() {
  //@ loop unroll 4;
  for (int i = 0; i < 5; i++);
}
