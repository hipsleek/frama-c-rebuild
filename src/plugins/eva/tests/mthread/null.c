/* run.config*
   STDOPT: +" -absolute-valid-range 100-200"
*/

#include <stddef.h>
#include <pthread.h>
#include <mthread.h>

pthread_t        tjob0, tjob1, tjob2;

volatile int v;


void * job0 (void *arg) {
  int i = v;
  *(char *)i = 1;
  return NULL;
}


int main() {
  int x = 3;

  pthread_create( &tjob0, NULL, &job0, NULL);

  int i = v;
  Frama_C_mthread_sync();  x = *(char *)i;

  return 0;
}
