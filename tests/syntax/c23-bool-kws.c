/* run.config
   EXIT: 0
   OPT: -print -std=c23
   OPT: -print -std=c23 -cpp-extra-args="-DUSE_Bool"
   OPT: -print -std=c23 -cpp-extra-args="-DUSE_Bool -DINCLUDE"
   OPT: -print -std=c23 -cpp-extra-args="-DINCLUDE"

   EXIT: 1
   OPT:

   EXIT: 0
   OPT: -print -cpp-extra-args="-DUSE_Bool"
   OPT: -print -cpp-extra-args="-DUSE_Bool -DINCLUDE"
   OPT: -print -cpp-extra-args="-DINCLUDE"
*/

#ifdef USE_Bool
#define BOOL  _Bool
#define TRUE  1
#define FALSE 0
#else
#define BOOL  bool
#define TRUE  true
#define FALSE false
#endif

#ifdef INCLUDE
#include <stdbool.h>
#endif

int main(void){
  BOOL b1 = FALSE ;
  BOOL b2 = TRUE ;
}
