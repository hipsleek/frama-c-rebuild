/* run.config
   STDOPT: +"-std=c11"
*/

#include <stdalign.h>

int main(void){
  int x = alignof(int);
  //@ assert alignof(int) == x ;
}
