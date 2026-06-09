/* run.config
STDOPT: #"-volatile-basetype -volatile-binding-auto -volatile-msg-key='binding'"
STDOPT: #"-volatile-basetype -volatile-binding-auto -volatile-binding c2fc2_Rd_INT"
STDOPT: #"-volatile-binding-auto -volatile-msg-key='binding'"
STDOPT: #"-volatile-binding-auto -volatile-binding c2fc2_Rd_INT"
*/

#line 1
typedef int INT;

volatile int x;
volatile INT y;

//@ requires \true;
extern int c2fc2_Rd_int (int volatile * p) ;

//@ requires \true;
extern INT c2fc2_Rd_INT (INT volatile * p) ;

//@ requires \true;
extern int c2fc2_Wr_int (int volatile * p, int x) ;


void f (void) {
  x = y ; /* TEST #1: c2fc2_Wr_int(&x,c2fc2_Rd_INT(&y))
	     TEST #2: idem */

  y = x ; /* TEST #1: c2fc2_Wr_int(&y,c2fc2_Rd_int(&))
	     TEST #2: c2fc2_Wr_int(&y,c2fc2_Rd_INT(&)) */

  /* So, the use of the option -volatile-binding is highly recommended.
     That allows to catch by the same function all volatile accesses
     of a given C-type what ever are the typename aliases for this C-type */
}


enum E1 { e1=0 } ;
enum E2 { e2=0 } ;
enum E3 { e3=0 } ;

volatile enum E1 x1;
volatile enum E2 x2;
volatile enum E3 x3;

//@ requires \true;
extern enum E1 c2fc2_Rd_enum_E1(enum E1 volatile *p);
//@ requires \true;
extern enum E2 c2fc2_Rd_enum_E2(enum E2 volatile *p);

int enum_volatile() {
  return x1 + x2 + x3;
}
