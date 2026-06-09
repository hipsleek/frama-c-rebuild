/* run.config
  COMMENT:
*/
/* run.config_qualif
  OPT: -wp-prop=-FAILS -wp-timeout 25
*/

struct S {
  int i ;
  int a[10] ;
};

void initialize(void){
  struct S s ;
  s.i = 0 ;
  /*@
    loop invariant 0 <= i <= 10 && \initialized(&s.a[0 .. i-1]);
    loop assigns i, s.a[0 .. 9];
    loop variant 10-i;
  */
  for(int i = 0; i < 10; ++i) s.a[i] = 0;

  //@ check \initialized(&s);
}

void range(void){
  struct S s ;
  s.i = 0 ;
  /*@
    loop invariant 0 <= i <= 10 && \initialized(&s.a[0 .. i-1]);
    loop assigns i, s.a[0 .. 9];
    loop variant 10-i;
  */
  for(int i = 0; i < 10; ++i) s.a[i] = 0;

  /*@
    loop invariant \initialized(&s);
    loop assigns i, s.a[1 .. 4];
    loop variant 10-i;
  */
  for(int i = 0; i < 10; ++i){
    if(1 <= i && i <= 4) s.a[i] = 1 ;
  }
  //@ check \initialized(&s);
}

void field(void){
  struct S s ;
  s.i = 0 ;
  /*@
    loop invariant 0 <= i <= 10 && \initialized(&s.a[0 .. i-1]);
    loop assigns i, s.a[0 .. 9];
    loop variant 10-i;
  */
  for(int i = 0; i < 10; ++i) s.a[i] = 0;

  /*@
    loop assigns i, s.i;
    loop variant 10-i;
    */
  for(int i = 0; i < 10; ++i){
    s.i++;
  }
  //@ check FAILS: \initialized(&s); // initialization not monotonic
}

void array(void){
  struct S s ;
  s.i = 0 ;
  /*@
    loop invariant 0 <= i <= 10 && \initialized(&s.a[0 .. i-1]);
    loop assigns i, s.a[0 .. 9];
    loop variant 10-i;
  */
  for(int i = 0; i < 10; ++i) s.a[i] = 0;

  /*@
    loop invariant 0 <= i <= 10;
    loop assigns i, s.a[0..9];
    loop variant 10-i;
  */
  for(int i = 0; i < 10; ++i){
    s.a[i] = 1 ;
  }
  //@ check FAILS: \initialized(&s); // initialization not monotonic
}

void index(void){
  struct S s ;
  s.i = 0 ;
  /*@
    loop invariant 0 <= i <= 10 && \initialized(&s.a[0 .. i-1]);
    loop assigns i, s.a[0 .. 9];
    loop variant 10-i;
  */
  for(int i = 0; i < 10; ++i) s.a[i] = 0;

  /*@
    loop assigns CHECK: i, s.a[4];
    loop variant 10-i;
    */
  for(int i = 0; i < 10; ++i){
    if(i == 4) s.a[i] = 1 ;
  }
  //@ check FAILS: \initialized(&s); // initialization not monotonic
}

void descr(void){
  struct S s ;
  s.i = 0 ;
  /*@
    loop invariant 0 <= i <= 10 && \initialized(&s.a[0 .. i-1]);
    loop assigns i, s.a[0 .. 9];
    loop variant 10-i ;
  */
  for(int i = 0; i < 10; ++i) s.a[i] = 0;

  /*@
    loop invariant \initialized(&s);
    loop assigns i, { s.a[i] | integer i ; i \in { 0, 2, 4 } };
    loop variant 10-i ;
  */
  for(int i = 0; i < 10; ++i){
    if(i == 0 || i == 2 || i == 4) s.a[i] = 1 ;
  }
  //@ check \initialized(&s);
}

void comp(void){
  struct S s ;
  s.i = 0 ;
  /*@
    loop invariant 0 <= i <= 10 && \initialized(&s.a[0 .. i-1]);
    loop assigns i, s.a[0 .. 9];
    loop variant 10-i;
  */
  for(int i = 0; i < 10; ++i) s.a[i] = 0;

  /*@
    loop invariant 0 <= i <= 10 ;
    loop assigns i, s;
    loop variant 10-i;
  */
  for(int i = 0; i < 10; ++i){
    s.a[i] = 1 ;
    s.i++;
  }
  //@ check FAILS: \initialized(&s); // initialization not monotonic
}
