/* run.config*
   STDOPT: +"-eva-builtin memcpy:Frama_C_memcpy -mt-verbose 3 -eva-warn-key builtins:override=inactive,builtins:missing-spec=inactive"
*/

// Make sure that Mthread respects options -val-builtin and -val-use-spec

#include <stddef.h>
#include <pthread.h>
#include <mthread.h>

pthread_t        tjob0, tjob1, tjob2;
int shared;
//@ assigns ((char*)a)[0..size-1], shared \from ((char*)b)[0..size-1];
void memcpy(void* a, void* b, unsigned long size) {
  for (int i=0; i<size; i++) {
    ((char*)a)[i] = ((char*)b)[i];
  }
}

int a, b, c, d;

void * job0 (void *v) {
  return NULL;
}

void * job1( void * k ) {
  Frama_C_mthread_sync(); int x = a+c;
  return NULL;
}

void * job2( void * k ) {
  int x = b+d;
  return NULL;
}

int main() {
  int x = 3;

  pthread_create( &tjob0, NULL, &job0, NULL);

  memcpy(&a, &x, sizeof(int));
  memcpy(&b, &x, sizeof(int));
  memcpy(&c, &x, sizeof(int));
  memcpy(&d, &x, sizeof(int));

  pthread_create( &tjob1, NULL, job1, NULL);
  pthread_create( &tjob2, NULL, job2, NULL);

  memcpy(&a, &x, sizeof(int));
  memcpy(&b, &x, sizeof(int));

  return 0;
}
