/* run.config
   COMMENT: This test used to make sure attributes were ignored when comparing
   COMMENT: function parameters, but we do not ignore them anymore. Now this
   COMMENT: test makes sure we can ignore an attribute when needed.
   STDOPT: +"-register-attributes unused:ignore"
*/

void f(int a __attribute__((unused)) );

int main(void){
  void (*op) (int) = f ;
  //@ assert op == f ;
}
