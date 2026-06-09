/* run.config
   COMMENT: taken from https://github.com/fraunhoferfokus/acsl-by-example/
   COMMENT: works thanks to Here-inlining
   STDOPT: +"-eva-unroll-recursive-calls 0"
*/
/* run.config_dev
   MACRO: ROOT_EACSL_GCC_OPTS_EXT --no-assert-print-data
   COMMENT: ptr values may change for each run
*/

/*@

  inductive CountInd{L}(int *arr, ℤ len, ℤ val, ℤ count)
  {
    case Nil{L}:
      \forall int *a, ℤ l, v;
        l <= 0  ==>  CountInd{L}(a, l, v, 0);

    case Hit{L}:
      \forall int *a, ℤ v, l, c;
        0 < l  &&  a[l-1] == v  &&  CountInd{L}(a, l-1, v, c)  ==>
        CountInd{L}(a, l, v, c + 1);

    case Miss{L}:
      \forall int *a, ℤ v, l, c;
        0 < l  &&  a[l-1] != v  &&  CountInd{L}(a, l-1, v, c)  ==>
        CountInd{L}(a, l, v, c);
  }

*/

int main() {
  int a[4] = {1, 2, 3, 4};
  int *ptr = &a;
  /*@ assert CountInd(ptr, 2, 2, 1); @*/
  /*@ assert CountInd(ptr, 2, 3, 0); @*/
  /*@ assert CountInd(ptr, 4, 3, 1); @*/
}
