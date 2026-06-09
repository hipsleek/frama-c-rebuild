/* run.config
STDOPT: #"-volatile-binding-auto -volatile-binding=Rd_int,Rd_unsigned_int -volatile-msg-key='binding'"
STDOPT: #"-volatile-basetype -volatile-binding-auto -volatile-binding=Rd_int,Rd_unsigned_int -volatile-msg-key='binding'"
*/

#line 1
volatile unsigned int x;
volatile unsigned int const cx;

//@ requires \true;
extern unsigned int c2fc2_Rd_unsigned_int (unsigned int volatile * p) ;

enum E1 { e1=0 } ;
enum E2 { e2=0 } ;

volatile enum E1 x1;
volatile enum E2 x2;

//@ requires \true;
extern enum E1 c2fc2_Rd_enum_E1(enum E1 volatile *p);

int unsigned_enum() {
  return x + cx + x1 + x2;
}

//@ requires \true;
extern unsigned int Rd_unsigned_int (unsigned int volatile * p) ;

//@ requires \true;
extern int Rd_int (int volatile * p) ;

enum ES1 { es1=-1 } ;
enum ES2 { es2=-1 } ;

volatile enum ES1 xs1;
volatile enum ES2 xs2;

//@ requires \true;
extern enum ES1 c2fc2_Rd_enum_wrong_name_ES1(enum ES1 volatile *p);

int signed_enum() {
  return x + cx + xs1 + xs2;
}

unsigned int y;
enum ES1 ys1 ;
void job (void) {
  x = xs1;
  y = xs2;
  ys1 = xs1;
}
