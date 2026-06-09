/* run.config
STDOPT: #"-volatile-basetype -volatile-binding='RdT1,RdT3,WrT3,RdE1,Rd_int'"
STDOPT: #"-volatile-basetype -volatile-binding='RdT2,RdT3,WrT3,RdE1,Rd_int'"
STDOPT: #"-volatile-basetype -volatile-binding='RdT1_bis,RdT3_bis,WrT3_bis,RdE1,Rd_int'"
STDOPT: #"-volatile-basetype -volatile-binding='RdT2_bis,RdT3_bis,WrT3_bis,RdE1,Rd_int'"
STDOPT: #"-volatile-basetype -volatile-binding='RdT1_ter,RdT3,WrT3,RdE1,Rd_int'"
STDOPT: #"-volatile-basetype -volatile-binding='RdT2_ter,RdT3,WrT3,RdE1,Rd_int'"
STDOPT: #"-volatile-binding='RdT1,RdT2,RdT3,WrT3,RdE1,RdE2,Rd_int'"
STDOPT: #"-volatile-binding='RdT1_bis,RdT2_bis,RdT3_bis,WrT3_bis,RdE1,RdE2_bis,Rd_int'"
STDOPT: #"-volatile-binding='RdT1_ter,RdT2_ter,RdT3,WrT3,RdE1,RdE2_ter,Rd_int'"
 */

#line 1
typedef int const *T1;
typedef int *T2;
typedef int * volatile T2_bis;

volatile T1 px;
volatile T2 py;
volatile T2_bis py_bis;
int const * volatile px_unrolled;
int * volatile py_unrolled;

//@ requires \true;
extern T1 RdT1 (T1 volatile * p) ;
//@ requires \true;
extern T1 RdT1_bis (int const * volatile * p) ;
//@ requires \true;
extern int const * RdT1_ter (T1 volatile * p) ;

//@ requires \true;
extern T2 RdT2 (T2 volatile * p) ;
//@ requires \true;
extern T2_bis RdT2_bis (T2_bis * p) ;
//@ requires \true;
extern T2 RdT2_ter (int * volatile * p) ;

int volatile_pointer () {
  return *px + *px_unrolled + *py + *py_unrolled+ *py_bis ;
}

typedef struct { int a ; volatile int x ; } T3 ;
typedef T3 T3_bis ;
typedef T3_bis T3_ter ;

//@ requires \true;
extern T3 RdT3 (T3 * p) ;
//@ requires \true;
extern T3_bis RdT3_bis (T3_ter * p) ;

//@ requires \true;
extern T3 WrT3 (T3 * p, T3 st) ;
//@ requires \true;
extern T3 WrT3_bis (T3_bis * p, T3_ter st) ;

T3 sx, sy;
void partial_access () {
  sx = sy ;
}

enum E1 { e1=0 } ;
enum E2 { e2=0 } ;
enum E3 { e3=0 } ;

typedef enum E2 E2_bis;
typedef E2_bis E2_ter;

//@ requires \true;
extern enum E1 RdE1(enum E1 volatile *p);
//@ requires \true;
extern enum E2 RdE2(enum E2 volatile *p);
//@ requires \true;
extern enum E2 RdE2_bis(E2_bis volatile *p);
//@ requires \true;
extern E2_ter RdE2_ter(enum E2  volatile *p);

volatile enum E1 x1;
volatile E2_ter x2;
volatile enum E3 x3;

int enum_volatile() {
  return x1 + x2 + x3;
}

//@ requires \true;
extern int Rd_int(int volatile *p);
