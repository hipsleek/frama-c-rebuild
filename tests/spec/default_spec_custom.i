/* run.config
   MODULE: @PTEST_NAME@
   STDOPT: +"-generated-spec-mode emptymode"
   STDOPT: +"-generated-spec-mode mymode"
   EXIT: 1
   STDOPT: +"-generated-spec-mode notregisteredmode"
*/

void f1(void);

void f2(){
  f1();
  return;
}

int f3(int* a);

int f4(int* b){
  return f3(b);
}
