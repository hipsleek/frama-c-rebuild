#include <stddef.h>
#include <pthread.h>

pthread_mutex_t  lock;
pthread_t        job;

int random(void);

void * fjob( void * k ) {
  pthread_mutex_lock(&lock);
  return NULL;
}

void main() {
  if (random ()) {
    pthread_mutex_init(&lock,NULL);

    if (random()) {
      pthread_mutex_lock(&lock);
    }
  }
  pthread_create(&job, NULL, &fjob, NULL);

}
