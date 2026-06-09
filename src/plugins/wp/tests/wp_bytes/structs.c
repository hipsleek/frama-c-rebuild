/* run.config*
   STDOPT: #"-cpp-extra-args=-DLE"
   STDOPT: #"-machdep=ppc_32"
*/

struct X {
  char  c ;
  short s ;
  int   i ;
  char  a[5] ;
} ;

struct Y {
  char c ;
  struct X x ;
} ;

//@ assigns *y ;
void callee(struct Y* y);

/*@ requires \separated(u, y) ;
    assigns  *y ;
    ensures  *u == \old(*u) ;
*/
void caller(struct Y const* u, struct Y* y){
  callee(y) ;
}

void loader_test(void){
  struct Y y ;
  unsigned char * ptr = &y ;

  /*@ loop invariant 0 <= i <= sizeof(y);
      loop invariant \forall integer j ; 0 <= j < i ==> ptr[j] == j ;
      loop assigns i, ptr[0 ..sizeof(y)-1];
      loop variant sizeof(y) - i ;
  */
  for(int i = 0 ; i < sizeof(y); i++){
    ptr[i] = i ;
  }
  //@ assert ptr[0] == 0;
  //@ assert ptr[1] == 1;
  //@ assert ptr[2] == 2;
  //@ assert ptr[3] == 3;
  //@ assert ptr[4] == 4;
  //@ assert ptr[5] == 5;
  //@ assert ptr[6] == 6;
  //@ assert ptr[7] == 7;
  //@ assert ptr[8] == 8;
  //@ assert ptr[9] == 9;
  //@ assert ptr[10] == 10;
  //@ assert ptr[11] == 11;
  //@ assert ptr[12] == 12;
  //@ assert ptr[13] == 13;
  //@ assert ptr[14] == 14;
  //@ assert ptr[15] == 15;
  //@ assert ptr[16] == 16;

#ifdef LE
  //@ check y.c == 0 ;
  //@ check y.x.c == 4 ;
  //@ check y.x.s == 7 * (1 << 8) + 6 ;
  //@ check y.x.i == 11 * (1 << 24) + 10 * (1 << 16) + 9 * (1 << 8) + 8 ;
  //@ check y.x.a[0] == 12 ;
  //@ check y.x.a[1] == 13 ;
  //@ check y.x.a[2] == 14 ;
  //@ check y.x.a[3] == 15 ;
  //@ check y.x.a[4] == 16 ;
#else
  //@ check y.c == 0 ;
  //@ check y.x.c == 4 ;
  //@ check y.x.s == 7 + 6 * (1 << 8) ;
  //@ check y.x.i == 11 + 10 * (1 << 8) + 9 * (1 << 16) + 8 * (1 << 24) ;
  //@ check y.x.a[0] == 12 ;
  //@ check y.x.a[1] == 13 ;
  //@ check y.x.a[2] == 14 ;
  //@ check y.x.a[3] == 15 ;
  //@ check y.x.a[4] == 16 ;
#endif
}
