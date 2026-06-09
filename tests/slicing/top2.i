/* run.config
* STDOPT: +"-slicing-level 2 -slice-annot main -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check "
* STDOPT: +"-slicing-level 2 -slice-return main -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check "
*/


int tab[2]={0, 7 };
int G, X ;
typedef struct {int a; int b; } Ts;
Ts S;

int f(void) {
 volatile int i=0;
 int v;

 v = tab[i];

 G = X;

 return v;
}

int main(void) {
 int x = f();
 G += 1 ;
 //@ slice_preserve_expr G ;
 return x;
}
