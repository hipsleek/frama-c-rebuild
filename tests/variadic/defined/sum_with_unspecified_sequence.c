/* run.config
STDOPT: +"-main sum -ulevel 1"
*/


#include <stdarg.h>

int sum(int n, ...){
  int ret = 0;
  int i;
  va_list list;
  va_start(list, n);
  for(i=0; i<n; i++){
    ret += va_arg(list, int);
  }
  va_end(list);
  return ret;
}
