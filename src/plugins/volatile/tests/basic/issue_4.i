/* run.config
STDOPT: #"-constfold"
*/
#line 1
extern int cpt;
/*@ assigns cpt ; ensures cpt == \old(cpt)+1; */
extern int rd (int volatile * p) ;

int volatile *A, *B;

//@ volatile *A, *B reads rd ;
int local_init(void) {
  int a = *A + *B;
  return a;
}

int labeled_stmt(int c, int a) {
  if (c) goto L;
  a++;
 L:a = a + *A + *B;
  return a;
}

int b;
int stmt_contract(int c, int a) {
  //@ ensures cpt == \at(cpt,Pre) + 2;
  a =  a + *A + *B;
  return a;
}
