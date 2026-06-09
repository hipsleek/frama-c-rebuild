/* run.config*
   DONTRUN:
*/

struct SAlF {
  int _Alignas(8) field ;
};

struct SAlF get_SAlF(void){
  return (struct SAlF){ 0 } ;
}

union UAlF {
  int _Alignas(8) field ;
};

union UAlF get_UAlF(void){
  return (union UAlF){ 0 } ;
}
