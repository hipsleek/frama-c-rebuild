/* run.config
   PLUGIN: @PTEST_PLUGIN@ eva,inout,scope
   OPT: -rte -then -print
   OPT: -eva -then -rte -then -print
   OPT: -eva -then -rte -rte-no-use-eva-results -then -print
*/

void f(int* p){
  *p = 42;
}

void g(int* p){
  *p = 42;
}

int main(void){
  int x ;
  f(&x);
}
