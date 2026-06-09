#include <stddef.h>
#include <pthread.h>


pthread_mutex_t*  lock;
pthread_t        th1;
pthread_t        th2;

void *t1 (void* _) {
  pthread_exit((void*) 1);
  return NULL;
}

void *t2 (void* _) {
  pthread_exit((void*) 2);
  return NULL;
}


int main () {
  pthread_create( &th1, NULL, t1, NULL);
  pthread_create( &th2, NULL, t2, NULL);
}
