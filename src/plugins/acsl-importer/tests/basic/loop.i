/* run.config
STDOPT: -acsl-import-unroll-loop-conditions -acsl-import %{dep:./loop.acsl} -print -acsl-import-debug 2 -acsl-import-msg-key "*"
 */
extern int f_extern(int i) ;

int while1 (int m, int v)
{
  int i = 0;
  int r = 0;

  while (r<v && i<m) {
   r=f_extern(i);
   i++;
  }
  return r;
}

int sum (int a, int b) {
  int i = 0 ;
  int r = 0 ;
  do {
    r++;
    i++;
  } while (i < a);
  for (int j = 0; j < b; j++)
    r++;
  return r ;
}

void job(int n) {
  int i = 0 ;
  {
    do i++ ;
    while (i < n) ;
  }

  {
    i = 0 ;
  L0:
    //@ assigns i;
  L1:
  L2: while (i < n)
      i++;
  }

  { //@ loop assigns \nothing ;
    for (;;) break ; }
  { for (;;) break ; }
  { for (;;i++) break ; }
  { for (;i < n;) break ; }
  { for (;i < n;i++) break ; }
  { for (i=1;;) break ; }
  { for (i=2;;i++) break ; }
  { for (i=3;i < n;) break ; }
  { for (i=4;i < n;i++) break ; }
  { for (int i=10;;) break ; }
  { for (int i=20;;i++) break ; }
  { for (int i=30;i < n;) break ; }
  { for (int i=40;i < n;i++) break ; }
}
