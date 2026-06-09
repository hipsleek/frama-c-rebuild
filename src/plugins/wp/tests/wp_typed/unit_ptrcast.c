#include <stdint.h>

/*@
  lemma pcast: \forall char *m; ((uintptr_t) m) >= 0;
*/

void locally(char* m){
  //@ assert ko: ((intptr_t) m) >= 0 ;
}
