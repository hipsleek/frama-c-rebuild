/* run.config
PLUGIN: eva,scope inout
    STDOPT: +"-eva -eva-verbose 6"
*/
//@ logic integer foo(int x) = x + 2 ;

int main() {
  int x = 42;
  //@ assert foo(x) >= x;
  return 0;
}
