/* run.config
   COMMENT: examples forcing the extraction into very specific situations
   STDOPT: +"-eva-unroll-recursive-calls 5"
*/

// extracts same function twice using different modes
/*@
  inductive multimode(ℤ x, ℤ y, ℤ z) {
      case zero: ∀ ℤ x; multimode(x, 0, 0);
  }

  inductive use_multimode(ℤ x,ℤ y) {
      case c: \forall ℤ a,b,c; multimode(0,b,0) ==> multimode(0,0,c) ==> use_multimode(a,b+c);
  }
*/

// Shows that sometimes even inductive predicates with a complex argument can
// be solved in complete mode without solving equations. Here n is substituted by i,
// rendering i+1 and i+2 immediately usable.
/*@
  inductive simple_complex_argument(ℤ i, ℤ x) {
      case one: \forall ℤ n; n ≡ 0 ==> simple_complex_argument(n, n+1);
      case two: \forall ℤ n; n ≡ 0 ==> simple_complex_argument(n, n+2);
  }
@*/

// shows generation of conditions instead of \let
/*@
  inductive conds(ℤ a, ℤ b, ℤ c) {
    case a: ∀ ℤ x, y; x <= 0 ∨ y <= 0 ⇒ conds(x,y,1);
    case b: ∀ ℤ x, y, z; conds(x-1,y,z) ==> conds(x,y-1,1) ==> conds(x,y,x-1);
  }
*/

// foreign incomplete predicate binding a variable that will be substituted
/*@
  inductive fibo(ℤ i, ℤ x) {
      case zero: fibo(0, 0);
      case one: fibo(1, 1);
      case other: \forall ℤ n, f1, f2; n>1 ==> fibo(n-1, f1) ==> fibo(n-2, f2) ==> fibo(n, f1+f2);
  }

  inductive use_var_bind_and_subst(ℤ x, ℤ y) {
      case c: \forall ℤ a; a >= 0 ==> fibo(0,a) ==> use_var_bind_and_subst(a,a+a);
  }

  inductive use_var_use_bind_and_subst(ℤ x, ℤ y) {
      case c: \forall ℤ a; fibo(0,a) ==> use_var_use_bind_and_subst(a,a+a);
  }
*/

/*@
  inductive bind_twice(ℤ a, ℤ b, ℤ c) {
    case a: ∀ ℤ x, y; x <= 0 ∨ y <= 0 ⇒ bind_twice(x,y,1);
    case b: ∀ ℤ x, y, z; bind_twice(0,y,z) ==> bind_twice(x,0,z) ==> bind_twice(x,y,x-1);
  }
*/

// let-bound variable are not to be substituted
/*@
  inductive subst_let(ℤ x, ℤ y) {
    case c: ∀ ℤ x; 0 < x ==> \let two = 2; x < two ==> subst_let(x, two);
    case d: subst_let(4, 2);
  }
*/

// Hypothesis gathering is interrupted by binding hypothesis.
// Leads to a duplication of constructor d.
/*@
  inductive dupl(ℤ a, ℤ b) {
    case c: ∀ ℤ x, y; x < 0 ⇒ dupl(1-x, y) ⇒ y < 9 ⇒ dupl(x, y+y);
    case d: ∀ ℤ x, y; x >= 0 ⇒ dupl(x, 1/x);
  }
*/

int main() {
  //@ assert use_multimode(0,0);
  //@ assert simple_complex_argument(0, 1);
  //@ assert simple_complex_argument(0, 2);
  //@ assert conds(0, 2, 1);
  //@ assert use_var_bind_and_subst(0,0);
  //@ assert use_var_use_bind_and_subst(0,0);
  //@ assert bind_twice(2, 3, 1);
  //@ assert subst_let(1,2);
  //@ assert dupl(-1, 1/2 + 1/2);
  return 0;
}
