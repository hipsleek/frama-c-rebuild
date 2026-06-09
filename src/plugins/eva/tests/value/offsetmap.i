/* run.config*
   STDOPT: #"-eva-ilevel 8 -no-warn-unaligned-pointer"
   STDOPT: #"-eva-ilevel 2 -no-warn-unaligned-pointer"
   STDOPT: #"-eva-warn-copy-indeterminate=-f,-g -no-warn-unaligned-pointer"
*/

int TT[10]={1,2,3};
int T[10]={1,2,3};
int i,a,b;
int a7, b7;

int O1[20];
int O2[20];
int *p;

int x2,*b2,a2;

void f() {
  for (i = 0; i <= 8; i++) {
    TT[i] = i;
    *((int*)((char*)&(TT[i]) + 1)) = 0;
  }

  a = 1;
  if (b) i=5; else i=6;
  a=3;
  if (i>=2) { a = i ; T[i] = 7 ; }

  for (i = 0; i <= 8; i++) {
    *(char *) &a = 1;
    b = a;

    *((int*)(((char*)&(T[i])) + 1)) = 0;
  }

  a7 = 'a';
  *(char *) &a7 = 1;
  b7 = (char)a7;

  ((int*)O1)[1]=17;
  ((char*)O1)[1]=18;

  ((int*)O2)[0]=10;
  ((char*)O2)[1]=11;

  O1[6]=0;
  p=O1+9;
  *p=1;

  x2 = 777;
  a2 = (int)&x2;
  b2 = (int*) a2;
  *((int*)a2) = 0;
  *b2=*b2+1;
}


int s[10000000];

/* Performance test on reading a value in an offsetmap. Here the offsetmap for s
   contains one value of 4 bytes repeated 10000000 times, and we read 1 byte at
   an unknown position in this offsetmap. 4 consecutive reads of 1 byte each are
   required to be sound. Doing 40000000 reads would be harshly inefficient
   (leading the analysis to not terminate on this function).  */
void g(int i) {
  s[i] = 0x1030807;
  char *p = &s[i];
  char c1 = *p;
  char *q = (char*)&s+i;
  char c2 = *q;
}

/*@ assigns \result \from min, max;
    ensures min <= \result <= max; */
int Frama_C_interval(int min, int max);

/* Test the soundness in offsetmaps when:
   - writing an isotropic value [v] (all bits are 0, or all bits are 1);
   - to an imprecise abstract location when all possible locations are
     contiguous and non-overlapping;
   - the number of possible locations is strictly greater than -eva-ilevel;
   - the target location contains a value with a different size or alignment
     than the write of [v]. */
void h(void) {
  int x = 257;
  /* Writing one byte to 0 in 4-byte integer x. */
  int i = Frama_C_interval(0, 3);
  char *p = (char *)&x + i;
  *p = 0;
  Frama_C_show_each_1_256_257(x); // Must at least contain 1, 256 and 257.
  /* Same operation on 4-byte pointer p. */
  int *q = &x;
  p = (char *)&q + i;
  *p = 0; // q is now completely invalid and should be a garbled mix.
  if (q != 0) *q = 42; // Thus there must be a memory access alarm here.
  /* Unaligned write in an array. */
  short t[8];
  //@ loop unroll 8;
  for (int j = 0; j < 8; j++) { t[j] = 257; }
  p = (char *)t + 1;
  i = Frama_C_interval(0, 6);
  short *sp = (short *)p + i;
  *sp = 0;
  Frama_C_show_each_1_256_257(t[4]); // Must at least contain 1, 256 and 257.
}

void main (int i) {
  f();
  g(i);
  h();
}
