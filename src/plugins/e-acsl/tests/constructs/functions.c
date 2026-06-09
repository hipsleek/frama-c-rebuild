/* run.config
   COMMENT: (recursive) logic functions and predicates without labels
   STDOPT: +"-eva-slevel 100 -eva-unroll-recursive-calls 100"
*/

/*@ predicate p1(int x, int y) = x + y > 0; */
/*@ predicate p2(integer x, integer y) = x + y > 0; */
int zero = 0;
/*@ logic integer f1(integer x, integer y) = x + y + zero; */

// E-ACSL integer typing:
// types less than int are considered as int
/*@ logic char h_char(char c) = c; */
/*@ logic short h_short(short s) = s; */

/*@ logic int g_hidden(int x) = x; */
/*@ logic int g(int x) = g_hidden(x); */

struct mystruct {
  int k, l;
};
typedef struct mystruct mystruct;
/*@ logic mystruct t1(mystruct m) = m; */
/*@ logic integer t2(mystruct m) = m.k + m.l; */

// To test function call in other clauses than assert:
/*@ predicate k_pred(integer x) = x > 0; */
/*@ requires k_pred(x); */
void k(int x) {}

// To test non-interference with global inits:
int glob = 5;

// To test that functions that are never called are not generated:
/*@ predicate never_called(int x) = x == x; */

/*@ logic double f2(double x) = (double)(1/x); */ /* handle in MR !226 */
//@ logic double f6(double* x) = (double)(1 / *x); // E-ACSL issue !236
// To test not_yet:
/*@ predicate p_labelled{L}(integer x) = x > 0; */
/*@ logic integer f_labelled{L}(integer x) = x; */

// Test sums inside functions
/*@ logic integer f_sum (integer x) = \sum(1,x,\lambda integer y; 1); */

// Test functions returning a rational
/*@ logic real over(real a, real b) = a/b; */

// Test functions using a rational
/*@ logic integer \signum(ℝ x) = x > 0. ? 1 : x < 0. ? -1 : 0; */

//Test function using a global variable (they elaborate to functions
//with labels)
int z = 8;
/*@ logic integer f3 (integer y) = z+y; */

/*@ predicate even (integer n) =
      n == 0 ? \true :
      n > 0 ? ! even (n - 1) :
      ! even (n + 1);
*/

/*@ predicate even_and_not_negative (integer n) =
      n == 0 ? \true :
      n > 0 ? ! even (n - 1) :
      \false;
*/

/*@ logic integer rf1(integer n) =
    n <= 0 ? 0 : rf1(n - 1) + n; */

/*@ logic integer rf2(integer n) =
    n < 0 ? 1 : rf2(n - 1)*rf2(n - 2)/rf2(n - 3); */

/*@ logic integer g(integer n) = 0; */
/*@ logic integer rf3(integer n) =
    n > 0 ? g(n)*rf3(n - 1) - 5 : g(n + 1); */

/*@ logic integer rf4(integer n) =
    n < 100 ? rf4(n + 1) :
    n < 0x7fffffffffffffffL ? 0x7fffffffffffffffL :
    6; */

/*@ logic integer rf5(integer n) =
  n >= 0 ? 0 : rf5(n + 1) + n; */

int main(void) {
  int x = 1, y = 2;
  /*@ assert p1(x, y); */;
  /*@ assert p2(3, 4); */;
  /*@ assert p2(5, 99999999999999999999999999999); */;

  /*@ assert f1(x, y) == 3; */;
  /*@ assert p2(x, f1(3, 4)); */;
  /*@ assert f1(9, 99999999999999999999999999999) > 0; */;
  /*@ assert f1(99999999999999999999999999999,
                 99999999999999999999999999999) ==
                 199999999999999999999999999998; */
  ;

  /*@ assert g(x) == x; */;

  char c = 'c';
  /*@ assert h_char(c) == c; */;
  short s = 1;
  /*@ assert h_short(s) == s; */;

  mystruct m;
  m.k = 8;
  m.l = 9;
  /*@ assert \let r = t1(m); r.k == 8; */;
  /*@ assert t2(t1(m)) == 17; */;

  k(9);

  double d = 2.0;
  /*@ assert f2(d) > 0; */;
  //@ assert f6(&d) > 0;
  /*@ assert f_sum (100) == 100; */;

  /*@ assert over(1., 2.) == 0.5; */;

  /*@ assert \at(p_labelled(27), Init); */;
  /*@ assert \at(f_labelled(27) == 27, Init); */;

  /*@ assert f3(5) == 13; */;

  /*@ assert \signum(3.0) > 0; */
  /*@ assert \signum(0.0-3.0) < 0; */
  /*@ assert \signum(0.0) ≡ 0; */

  /*@ assert even(10); @*/;
  /*@ assert even(-6); @*/;
  /*@ assert even_and_not_negative(10); */;
  /*@ assert rf1(0) == 0; */;
  /*@ assert rf1(1) == 1; */;
  /*@ assert rf1(10) == 55; */;

  /*@ assert rf2(7) == 1; */;

  /*@ assert rf3(6) == -5; */;

  /*@ assert rf4(9) > 0; */;

  /*@ assert rf5(0) == 0; */

  /*@ assert (\let n = (0 == 0) ? 0x7fffffffffffffffL : -1; rf5(n) == 0);*/
}

// Function with different return types depending on the call site
// The recursive call is in a GMP context
// ⇒ the result needs to be passed back via an additional argument as a reference.
// The external call is in an int context ⇒ the result is passed back normally using return.
// This regression test there is to ensure that the two cases are not mixed
// together resulting in the wrong number of arguments being passed to the function.
/*@ logic ℤ f4 (ℤ x) = x ≡ 0 ? 0 : f4(x-1) < 1<<99 ? 1 : 2; */

void test_f4() {
  /*@ assert f4 (0) ≡ 0; */
}

// same term \at(j, Old) twice in different typing contexts
/*@
  behavior a:
    ensures 1 == -j;
  behavior b:
    ensures 2 == j;
 */
int f5(long int j) {}

int test_f5() {
  f5(1);
}
