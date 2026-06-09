/*@
  region A: p[0..n-1] , \nullable, \garbage ;
  region B: q[0..n-1] , \nullable, \readonly ;
  */
int job_disjoint( int n, int * p , int * q )
{
  if (!p) return 0;
  if (!q) return 0;
  for (int k=0; k<n; k++) p[k] = q[k];
  return 1;
}

/*@
  region A: p[0..n-1] , \nullable, \garbage ;
  region B: q[0..n-1] , \nullable, \readonly ;
  region p[..], q[..];
  */
int job_merged( int n, int * p , int * q )
{
  if (!p) return 0;
  if (!q) return 0;
  for (int k=0; k<n; k++) p[k] = q[k];
  return 1;
}

/*@ region x, \garbage ; */
void formal_garbage_ok(struct S { int f; } x);

/*@ region x, \garbage ; */
void formal_garbage_ko(int x);
