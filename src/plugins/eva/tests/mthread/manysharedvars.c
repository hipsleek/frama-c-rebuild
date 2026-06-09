/* run.config*
   STDOPT:
   STDOPT: +"-eva-slevel 150"
*/

/* This example tests the behavior of Mthread with many shared vars,
   here an array with a medium number of cells. */

#include <stddef.h>
#include <pthread.h>
#define N 128

struct pair {
  int data;
  int data2;
};


struct pair s[N];

pthread_t        jobs;
pthread_mutex_t  lock;

int random(void);

void *f(void *_) {
  for (int i=0; i<N; i++) {
    pthread_mutex_lock(&lock);
    s[i].data++;
    s[i].data2 += 3;
    pthread_mutex_unlock(&lock);
  }
  return NULL;
}




void main(void)
{
  int t=0;
  pthread_mutex_init( &lock, NULL);
  pthread_create( &jobs , NULL, f, NULL);

  for (int i=0; i<N; i++) {
    pthread_mutex_lock(&lock);
    t += s[i].data;
    t += s[i].data2;
    pthread_mutex_unlock(&lock);
  }
}
