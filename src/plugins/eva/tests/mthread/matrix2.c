/*
  THIS FILE IS USED AS AN EXAMPLE FOR THE WEBSITE. DO NOT FORGET TO UPDATE THE
  EXAMPLES ON THE WEBSITE IF IT IS EDITED.
  When updating the examples, copy the file in a new folder, remove comments
  until the ---- line and then run the following command to generate the log
  file and the HTML summary:

  frama-c -mthread -mt-threads-lib pthreads -mt-shared-values 2 \
    -mt-shared-accesses-synchronization \
    -eva-verbose 0 -mt-extract html \
    -eva-slevel 15 matrix2.c > output.txt
*/
/* -------------------------------------------------------------------------- */
#include <stddef.h>
#include <pthread.h>
#define S 150
#define N 5

pthread_mutex_t  locks[N];
pthread_t        jobs[N];

unsigned int matrix[S];

unsigned int compute(int i, unsigned int prev);
int completed(unsigned int sum);

void * job( void * k ) {
  int i = (int) k ;

  while(1) {
    pthread_mutex_lock(locks+i);
    for(int j=i; j<S; j+=N)
      matrix[j] = compute(j, matrix[j]);
    pthread_mutex_unlock(locks+i);
  }
}

int main() {
  int i, j, sum;
  sum = 0;

  for(i=0;i<N;i++)
    pthread_mutex_init( &locks[i] , NULL);

  for(i=0;i<N;i++)
    pthread_create( &jobs[i], NULL, &job, (void *) i );

  while (!completed(sum)) {
    sum = 0;
    //@ loop unfold N;
    for (i=0; i<N; i++) {
      pthread_mutex_lock(locks+i);
      int* pj = matrix+i;
      while(pj < matrix+S) {
        sum += *pj;
        pj += N;
      }
      pthread_mutex_unlock(locks+i);
    }
  }
  return 0;
}
