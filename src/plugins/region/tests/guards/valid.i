/* run.config
  STDOPT:
  STDOPT: +"-unsafe-arrays"
  STDOPT: +"-no-warn-unaligned-pointer"
*/

struct S { int f[5]; };

//@ region *p ;
int ptr (int *p) { return *p; }

//@ region p[0..n-1] ;
int arr (int *p, int k, int n) { return p[k]; }

//@ region p;
int comp (struct S p, int k) { return p.f[k]; }

//@ region *p;
int pcomp (struct S *p, int k) { return p->f[k]; }

//@ region p[0..n-1];
int cast (char *p, int k, int n) { return *((int*)(p + k)); }

//@ region ((int*)p)[0..n-1];
int nocast (char *p, int k, int n) { return *((int*)p + k); }
