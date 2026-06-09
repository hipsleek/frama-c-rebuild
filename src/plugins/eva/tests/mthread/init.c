/* run.config*
   STDOPT: +"-pp-annot -mt-threads-lib builtins-only"
*/
/* This example tests the various way a structure can be named:
   with a pointer, with a string, without any indication */
#include <stddef.h>
#include <mthread.h>
#define N 3

int  locks[N];
char (*names[2*N]) = { "mu1", "mu2", "mu3", "mu4", "mu5", "mu6" };


int mutex_init(void* mname) {
  return Frama_C_mutex_init(mname);
}

void main() {
  int i ;

  for(i=0;i<N;i++)
    mutex_init(&locks[i]);

  for(i=0;i<N;i++)
    mutex_init(names[i]);

  // Warning: the same mutex is repeatedly created
  for(i=0;i<N;i++)
    mutex_init(NULL);
}
