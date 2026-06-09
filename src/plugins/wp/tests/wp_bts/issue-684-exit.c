#include "stdlib.h"
/*@ assigns \nothing;
  @ exits \exit_status == state;
  @ ensures \false;
*/
__attribute__((noreturn)) void unconditional_exit(int state) {
  exit (state);
}
