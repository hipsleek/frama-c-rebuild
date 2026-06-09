/* run.config
   STDOPT: #"-volatile-binding-call-pointer"
   STDOPT: #"-volatile-call-pointer F,G"
*/
#line 1
//@ assigns \nothing;
extern int rd (int volatile * p) ;

//@ assigns \nothing;
extern int wr (int volatile * p, int v) ;

//@ assigns \nothing;
extern int c2fc2_Call_int_unsigned_int_int(int (*)(unsigned,int),unsigned,int);

//@ assigns \nothing;
extern int c2fc2_Call_int_int_int(int (*)(int,int),int,int);

// Type Mismatch!
//@ assigns \nothing;
extern int c2fc2_Call_int_double_double(int (*)(double,double),float,float);

//@ assigns \nothing;
extern int F(int (*)(int,int),int,int);

//@ assigns \nothing;
extern int G(int (*)(unsigned,int),unsigned,int);

unsigned int A;
int volatile B;
int volatile C;

//@ volatile B, C reads rd writes wr;

int job_normal(int (*f)(int,int),int a,int b)
{
  return (*f)(a,b);
}

int job_mismatch(int (*f)(double,double),double a,double b)
{
  return (*f)(a,b);
}

void job_volatile(int (*f)(unsigned,int))
{
  C = f(A,B);
}
