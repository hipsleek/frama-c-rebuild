/*@ ghost
    /@ assigns \result \from \nothing; @/
    int *f(void);
*/

/*@ ghost
  /@ assigns \result,*\result \from \nothing; @/
  int \ghost * g(void);
*/

int main(void){
  //@ ghost int* p = f() ;
  //@ ghost int \ghost* q = g() ;
}
