#include <stddef.h>
#include <pthread.h>

pthread_mutex_t  lock1, lock2;
pthread_t   tjob;

int t[10];

void * job( void * k ) {
  int s = 0;
  for (int i=0; i<10; i++) {
    s += t[i];
  }
  return NULL;
}

int main() {

  pthread_mutex_init( &lock1 , NULL);
  pthread_mutex_init( &lock2 , NULL);

  pthread_create( &tjob, NULL, &job, NULL );

  pthread_mutex_lock(&lock1);
  t[1] = 1;
  pthread_mutex_unlock(&lock1);

  pthread_mutex_lock(&lock2);
  t[3] = 4;
  pthread_mutex_unlock(&lock2);

  return 0;
}
