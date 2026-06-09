/* run.config
 LIBS: libSelect
 MODULE: @PTEST_NAME@
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main

 MODULE:
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-annot main -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-assert main -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-annot modifS -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-annot f1 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-annot f2 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-annot f3 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-annot f4 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-annot f5 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-annot f6 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-annot f7 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-loop-inv f8 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-annot f8 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-assert f8 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
   OPT: @EVA_OPTIONS@ -deps -lib-entry -main main -slice-annot f9 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps

*/
struct Tstr { int a; int b; } S;
int Sa ;

int f1(int cond) {
  int * p = &S.a ;
  if (cond) {
    //@ assert (cond != 0);
    Sa = *p ;
    }
  //@slice_preserve_expr *p;
  return Sa ;
}

int f2(int cond) {
  int * p = &S.a ;
  if (cond)
    //@ assert (cond != 0);
    Sa = *p ;
  //@slice_preserve_expr S.a;
  return Sa ;
}

int f3(int cond) {
  int * p = &S.a ;
  if (cond) {
    //@ slice_preserve_ctrl;
    Sa = *p ;
    }
  return Sa ;
}

int f4(int cond) {
  int * p = &S.a ;
  if (cond) {
    //@ slice_preserve_stmt;
    Sa = *p ;
    }
  return Sa ;
}

int f5(int cond) {
  int * p = &S.a ;
  if (cond) {
    //@ slice_preserve_expr 1;
    Sa = *p ;
    }
  return Sa ;
}

int f6(int cond) {
  int * p = &S.a ;
  //@ slice_preserve_stmt;
  if (cond) {
    Sa = *p ;
    Sa ++ ;
    }
  return Sa ;
}

int f7(int cond) {
  int * p = &S.a ;
  if (cond)
  //@ slice_preserve_stmt;
    {
      Sa = *p ;
      Sa ++ ;
    }
  return Sa ;
}


int f8(int cond) {
  int * p = &S.a ;
  //
  /*@ loop invariant cond >= 0 ;
    loop variant cond ; */
  while (cond)
    { //@ assert  cond <= \at(cond,Pre) ;
      //  assert S.a + cond == \at(S.a + cond,Pre) ;
      Sa = *p ;
      //@ slice_preserve_stmt;
      S.a ++ ;
      cond--;
    }
  return Sa ;
}

int X9, Y9, Z9 ;
void f9(int c1, int c2) {
  if (c1 > c2)
    goto L;
  c1 = c2 ;
  //@ slice_preserve_stmt;
  {L: X9 = c1 ;}
  Y9 = Z9 ;
  Z9 = c2 ;
}

void modifS (int a, int b) {
  S.a += a;
  S.b -= b;
  //@slice_preserve_expr S.a;
}
int new_int (void);
  int d;
int main (void) {
  int a = 0;
  int b = 0;
  int c = 0;
  if (d > 0) {
    //@ assert (b == 0);
    a = 1;
    }
  //@ slice_preserve_expr a+b;
  int x = a+b+c+d;
  modifS (a, b);
  // assert (d>0 => a == 1) && (!(d>0) => a==0);
  d = new_int ();
  f1(d) ;
  f2(d) ;
  f3(d) ;
  f4(d) ;
  f5(d) ;
  f6(d) ;
  f7(d) ;
  f8(d) ;
  f9(d,a) ;
  return x;
}
