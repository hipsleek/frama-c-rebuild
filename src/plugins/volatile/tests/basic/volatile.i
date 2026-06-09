/* run.config
STDOPT: #"-machdep x86_32 -volatile-basetype -volatile-binding-auto"
STDOPT: #"-machdep x86_32 -volatile-basetype -volatile-binding-auto -volatile-warn-key transformed-access=inactive"
STDOPT: #"-machdep x86_32 -volatile-binding-auto"
STDOPT: #"-machdep x86_32 -volatile-binding-auto -volatile-warn-key transformed-access=inactive"
 */

#line 1
typedef volatile int VINT ;
extern int f (int volatile * p) ;
extern int g (VINT * p, int v) ;

typedef int INT10[10] ;
VINT x, *p, **q;
VINT t[10] ;
VINT tt[2][3] ;
volatile INT10 t10 ;
typedef int INT10_2[2][10] ;
volatile INT10_2 t10_2 ;

typedef struct st1 { int a; int b;} STT;
typedef struct st2 { int a; int b; int t[10]; STT s;} ST;
struct st3 { VINT a; int b; int t[10];} s;
typedef struct st3 SV;
volatile ST sv;

extern ST c2fc2_Rd_struct_st2 (volatile ST * p) ; 
extern SV gs (SV * p, SV v) ;
extern ST gsv (volatile ST * p, ST v) ;

//@volatile x,t[..],s.a,tt[..][..] reads f ;
//@volatile *p,**q,tt[..][..],t10[..],t10_2[..][..] writes g ;
//@volatile *p reads f ;
//@volatile sv writes gsv ;
//@volatile s  writes gs ;
ST (*pf)(volatile ST *) = c2fc2_Rd_struct_st2 ;

int y;
unsigned int u;
int main (void) {
  ST * pst = (ST *) &sv;
  pst->b = 4;
  //x;
  x = s.a ;
  sv.a = x ;
  s = s ;
  sv = sv ;
  **q = 3 ;
  *(p+1) = 4 ;
  p[2] = 4 ;
  *(t+y)=0;
  t[1] = y;
  t10[1] = y;
  t10_2[1][2] = y;
  tt[2][3] = y;
  y = sv.t[1];
  y = sv.s.a;
  u = sizeof (x);
  return x ;
}

extern int k(int a, int b) ;
int wr (void) {
  s.a = x ;
  if (x)
    *p = x ;
  *(p+x) = 3 ;
  //@ ensures *p > 0 ;
 l:*p = k(1, 2) ;
  if (x)
    return 3 ;
  return 1 ;
}

//@ volatile *((VINT *)0x2) reads f;
int numerical (void) {
  int tmp ;
  tmp = *((VINT *)0x2);
  return tmp;
}
