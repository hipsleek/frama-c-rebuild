#include <stddef.h>
#include <pthread.h>
#define N 5
#define M 6
#define K 100

unsigned int matrix[N*M][K];
unsigned int psum[K];

extern int result;

pthread_mutex_t  locks[N];
pthread_t        jobs[N];

int random(void);
unsigned int f(short i, unsigned int v);


void * job( void * k ) {
  short i = (short) k ;
  Frama_C_show_each(i);

  while (1) {
    pthread_mutex_lock(&locks[i]);
    for (int j=0; j<K; j++)
      //@ loop unfold M;
      for(int l=0; l<M; l++)
        matrix[i+N*l][j] = f(i,matrix[i][j]);
    pthread_mutex_unlock(&locks[i]);
  }
}

int main(void) {
  int i, j, l;

  for(i=0; i<N; i++)
    pthread_mutex_init(&locks[i] , NULL);

  for(i=0; i<N; i++)
    for(j=0; j<K; j++)
      for(l=0; l<M; l++)
        matrix[i][j] = 0;

  for(i=0;i<N;i++)
    pthread_create( &jobs[i], NULL, &job, (void *) i );

  int sum = 0;
  while (sum != result) {
    for(j=0; j<K; j++) {
      psum[j]=0;
    }
    //@ loop unfold N;
    for(i=0; i<N; i++) {
      pthread_mutex_lock(&locks[i]);
      for(j=0; j<K; j++)
        //@ loop unfold M;
        for(l=0; l<M; l++)
          psum[j] += matrix[i+N*l][j];
      pthread_mutex_unlock(&locks[i]);
    }
    sum = 0;
    for(j=0; j<K; j++) {
      sum += psum[j];
    }
  }

  return 0;
}
