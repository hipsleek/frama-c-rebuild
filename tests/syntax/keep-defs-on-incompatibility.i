/* run.config
   EXIT: 1
   STDOPT: #"%{dep:./keep-defs-on-incompatibility-companion.i}"
*/

struct S {
  char field ;
};

struct S get_S(void);

void use(void){
  struct S s = get_S() ;
}
