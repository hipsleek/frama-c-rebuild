#include <stddef.h>
#include <pthread.h>

pthread_t job1;

int __fc_random_counter __attribute__((unused)) __attribute__((FRAMA_C_MODEL));

/*@ assigns \result \from __fc_random_counter ;
  @ assigns __fc_random_counter \from __fc_random_counter ;
*/
int rand(void);

void *f1(void * p) {
  int x = rand();
  return NULL;
}

void main() {

  pthread_create(&job1, NULL, f1, NULL);

  int y = rand();
}
