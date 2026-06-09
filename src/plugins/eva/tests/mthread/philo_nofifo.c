/* Simple dining philosophers example.
   No messages are exchanged between the merry company at the table.
*/
#include <stddef.h>
#include <pthread.h>
#define N 5

pthread_mutex_t  locks[N];
pthread_t        jobs[N];

void aux (int l, int r, int mess) {
  pthread_mutex_lock(locks+l);
  pthread_mutex_lock(locks+r);
  pthread_mutex_unlock(locks+r);
  pthread_mutex_unlock(locks+l);
}

void * job( void * k ) {
  int p = (int) k ;
  int l = p>0 ? p-1 : N-1 ;
  int r = p<N-1 ? p+1 : 0 ;

  while(1)
    aux(l, r, p+1);
}

int main() {
  int i ;

  for(i=0;i<N;i++)
    pthread_mutex_init( &locks[i] , NULL);

#ifdef SPIN
  //@ loop unfold N;
#endif
  for(i=0;i<N;i++)
    pthread_create( &jobs[i], NULL, &job, (void *) i );

  return 0;
}
