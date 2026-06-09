/* run.config
   DONTRUN:
*/

/* run.config_qualif
   DEPS: rnn.mlw
   OPT: -wp-model real -wp-library @PTEST_DIR@
*/

/*
   RNN Implementation in C
    - Inputs & Outputs are floats
    - The state is a double for better precision
    - Cell is a weight combination of state and input
    - Output is simply RELU of state

*/

#define W 0.5
#define X 2.0

/*@
  import why3: rnn::Spec;
  axiomatic Vector {
    predicate IsVector{L}(float *a, integer n, Spec::vector v) =
      \forall integer k;
      0 <= k < n ==> \at(a[k],L) == Spec::get(v,k);
  }
  */

/*@
  requires 0 <= n ;
  requires \separated( x + (0..n-1) , y + (0..n-1) );
  assigns y[0..n-1];
  ensures
    \forall Spec::vector xs, ys;
      IsVector(x,n,xs) ==>
      IsVector(y,n,ys) ==>
      Spec::eqn( ys, Spec::rnn(xs), n );
  */
void rnn(float *x, float *y, int n)
{
  double h = 0.0;
  /*@
    loop invariant 0 <= k <= n ;
    loop invariant Hidden:
      \forall Spec::vector xs;
      IsVector(x,n,xs) ==> h == Spec::hidden( xs, k );
    loop invariant Output:
      \forall Spec::vector xs;
      IsVector(x,n,xs) ==>
      \forall integer j;
      0 <= j < k ==>
      y[j] == Spec::get( Spec::rnn(xs), j );
    loop assigns k, h, y[0..n-1];
    loop variant n-k;
    */
  for (int k = 0; k < n; k++) {
    h = W * h + X * (double) x[k] ;
    /*@ assert h == Spec::state( \at(h,LoopCurrent), x[k] ); */
    y[k] = h > 0.0 ? h : 0.0 ;
    /*@ assert
        \forall Spec::vector xs;
          IsVector{Here}(x,n,xs) <==> IsVector{LoopCurrent}(x,n,xs) ;  */
  }
}
