/* run.config*

 EXIT: 1
   OPT:
*/
void f(void) {}

int main()
  {
  void (*p)(void) = &f ;
  p = &f ;
  *p = f ;
  return 0 ;
  }
