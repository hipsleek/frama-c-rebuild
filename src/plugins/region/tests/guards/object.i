/* run.config
   STDOPT:
   STDOPT: +"-warn-invalid-pointer"
*/

//@ region p[0..n-1];
int access(int *p, int k, int n)
{
  p+= k;
  return *p;
}
