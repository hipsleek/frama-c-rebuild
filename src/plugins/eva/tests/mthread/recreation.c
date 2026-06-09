/* This example initializes an important number of thread in a convoluted
   manner (all thread uses the function main, but depending on their
   argument, they create other threads or not) */

#include <stddef.h>
#include <pthread.h>
#define N 3
#define P N*N+N

int end = 0 ;
pthread_t        jobs[P];
pthread_mutex_t  locks[P];


int random(void);

void* job(void* k)
{
  int p = (int) k ;
  int i;

  if (p < N) {
    for (i=0;i<N;i++) {
      int j = (p+1)*N+i;
      pthread_create(&jobs[j], NULL, &job, (void*)j);
    }

    while(!end) {};
  }
  else {

    int l = p>0 ? p-1 : P-1 ;
    int r = p<P-1 ? p+1 : 0 ;


    while(1) {
      pthread_mutex_lock( locks+l );
      pthread_mutex_lock( locks+r );

      if (random()) { end = 1;}

      /* If enabled, causes a problem with mutex locking */
      if (random()) {
        pthread_mutex_lock( locks);
        pthread_mutex_unlock( locks);
      }

      pthread_mutex_unlock( locks+l );
      pthread_mutex_unlock( locks+r );
    }
  }
  return NULL;
}


void* job2(void *);

int main()
{

  int i ;
  for(i=0;i<P;i++)
    pthread_mutex_init(&locks[i], NULL);

  for(i=0;i<N;i++)
    pthread_create( &jobs[i] , NULL, &job, (void *) i );

  while(!end) ;

  return 0;
}

