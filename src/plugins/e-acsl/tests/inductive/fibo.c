/* run.config
   COMMENT: based on src/plugins/wp/tests/wp_plugin/tutorial.i
   STDOPT: +"-eva-unroll-recursive-calls 9"
*/

// translatable in incomplete mode 1
/*@
  inductive fibo(ℤ i, ℤ x) {
      case zero: fibo(0, 0);
      case one: fibo(1, 1);
      case other: \forall ℤ n, f1, f2;
          n>1 ==> fibo(n-1, f1) ==> fibo(n-2, f2) ==> fibo(n, f1+f2);
  }
*/

// same predicate using two complex arguments in the conclusion
// translatable in incomplete mode 1 thanks to algebraic inversion
/*@
  inductive fibo2(ℤ i,ℤ x) {
      case zero: fibo2(0,0);
      case one: fibo2(1,1);
      case other: \forall ℤ n, f1, f2;
          n>0 ==> fibo2(n,f1) ==> fibo2(n-1,f2) ==> fibo2(n+1,f1+f2);
}
*/

// not translatable
/*@
  inductive is_fibo(ℤ x) {
      case zero: is_fibo(0);
      case one: is_fibo(1);
      case other: \forall ℤ a, b; is_fibo(a) ==> is_fibo(b) ==> is_fibo(a+b);
  }
*/

// translatable in complete mode using foreign predicate fibo in incomplete mode 1
int one = 1;
/*@
  inductive lucas(ℤ i, ℤ x) {
      case zero: lucas(0, 2);
      case more: \forall ℤ n, f1, f2; fibo(n-one, f1) ==> fibo(n+1, f2) ==> lucas(n, f1+f2);
  }
*/

int main() {
  /*@ assert fibo(7, 13); @*/
  /*@ assert fibo2(7, 13); @*/
  /*@ assert is_fibo(13); @*/

  /*@ assert lucas(0, 2); @*/
  /*@ assert lucas(1, 1); @*/
  /*@ assert lucas(2, 3); @*/
  /*@ assert lucas(3, 4); @*/
  /*@ assert lucas(4, 7); @*/

  /*@ assert !fibo(7, 12); @*/
  /*@ assert !fibo2(7, 12); @*/
  /*@ assert !lucas(4, 5); @*/
  return 0;
}
