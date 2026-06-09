/* run.config*

   MACRO: SRC1 @PTEST_NAME@.0.res.log
   MACRO: SRC2 @PTEST_NAME@.1.res.log
   MACRO: DIFF @PTEST_NAME@.variadic.diff

   STDOPT: +"-no-variadic-translation"
   STDOPT:

   COMMENT: The two outputs should be identical
   EXECNOW: LOG @DIFF@ diff %{dep:@SRC1@} %{dep:@SRC2@} > @DIFF@
*/

#include <stddef.h>
#include <pthread.h>
#include <__fc_builtin.h>

#define N 2
pthread_mutex_t locks[N];
pthread_t jobs[N];
int vars[N];

void * job(void * arg) {
  int i = (int) arg;
  pthread_mutex_lock(&locks[i]);
  vars[i] += Frama_C_interval(0, 10);
  pthread_mutex_unlock(&locks[i]);
  return NULL;
}

int main() {
  for (int i = 0 ; i < N ; ++i) {
    pthread_mutex_init(&locks[i], NULL);
  }

  for (int i = 0 ; i < N ; ++i) {
    pthread_create(&jobs[i], NULL, job, (void *) i);
  }

  int sum = 0;
  for (int i = 0 ; i < N ; ++i) {
    pthread_mutex_lock(&locks[i]);
  }
  for (int i = 0 ; i < N ; ++i) {
    sum += vars[i];
  }
  for (int i = 0 ; i < N ; ++i) {
    pthread_mutex_unlock(&locks[i]);
  }

  return sum;
}
