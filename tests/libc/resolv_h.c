/*run.config
  STDOPT:
*/

#include "__fc_builtin.h"
#include <resolv.h>

#ifndef NS_PACKETSZ
#define NS_PACKETSZ  512
#endif

int main() {
  // test code based on hesiod
  unsigned char qbuf[NS_PACKETSZ], abuf[NS_PACKETSZ];
  int n, i = Frama_C_interval(0, 512), len = 512;
  if ((_res.options & RES_INIT) == 0 && res_init() == -1) return 1;
  n = res_send(qbuf, i, abuf, len);
  if (n < len) {
    return 1;
  }
  return 0;
}
