/* run.config*
 STDOPT:
 EXIT: 1
   STDOPT: #"-cpp-extra-args=-DMISSING_COMPUTATION"
   STDOPT: #"-cpp-extra-args=-DOUTSIDE_FDEC"
 */

/* This is expected to work */
void f(int x){
  ({;});
}

#ifdef OUTSIDE_FDEC
  int x = ({ 42; });
#endif

#ifdef MISSING_COMPUTATION
int main(int x){
  x = x + ({;});
}
#endif
