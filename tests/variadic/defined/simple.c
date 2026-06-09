/* run.config
STDOPT:
STDOPT: #"-no-frama-c-stdlib -no-pp-annot"
*/

/* The defines and typedefs below avoid issues with Musl: without them,
   pretty-printing 'va_list' results in `typedef __gnuc_va_list va_list`, but
   only on GNU libc-based systems (and not on Alpine Linux, which is based on
   musl). */
#define __GNUC_VA_LIST
#define __va_list__
typedef void *__builtin_va_list;
typedef __builtin_va_list va_list;
#include <stdarg.h>



/*@ requires n>= 0; */
int sum(int n, ...){
  int ret = 0;
  int i = 0;
  va_list list;
  va_start(list, n);

  for(i; i<n; i++){
    ret += va_arg(list, int);
  }

  va_end(list);
  return ret;
}

int main(){
  return sum(5, 6, 9, 14, 12, 1);
}

