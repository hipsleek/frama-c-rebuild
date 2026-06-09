/* Tests Eva analyses with kernel 'loop unfold' annotations. */

int volatile nondet;

enum { NB_TIMES=12, FIFTY_TIMES = 50 };

void main(int c) {
  int G=0,i;
  int MAX = 12;
  int JMAX=5;
  int j,k,S;
  /*@ loop unfold 14; */ // first loop unrolled 14 times
  for (i=0; i<=MAX; i++)
    {
      G+=i;
    }
  /*@ loop unfold 124; */
  for (i=0; i<=10*MAX; i++)
    {
      G+=i;
    }
  /*@ loop unfold 12+2; */ // loop unrolled 14 times
  for (i=0; i<=MAX; i++)
    {
      j=0;
      /*@ loop unfold FIFTY_TIMES; */
      while (j<=JMAX)
        {
          G+=i;
          j++;
          }
    }

//@ loop unfold 128*sizeof(char);
  do {
    G += i;
    i++;
    j--;
    }
  while (i<=256 || j>=0);

//@ loop unfold 10;
 do
    { if(c) continue;

    if(c--) goto L;
    c++;
  L: c++;
      }
  while(c);

//@ loop unfold c;
 while(0);

 S=1;
 k=1;
 //@ loop unfold "completely", NB_TIMES;
 do {
   S=S*k;
   k++;
 } while (k <= NB_TIMES) ;

 if (nondet) {
   /* The loop is not completely unrolled with NB_TIMES iteration:
      the loop invariant false introduced by "completely" is reachable. */
   //@ loop unfold "completely", NB_TIMES;
   for (int i = 0; i <= NB_TIMES; i++);
  }
}
