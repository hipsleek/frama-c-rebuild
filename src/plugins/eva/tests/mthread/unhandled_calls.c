/* run.config*
   EXIT: 1
   STDOPT: @PTEST_SHARE_DIR@/mthread/mthread_queue.c
 */
/* This example tests a very specific error message within message, a call
   through a function pointer calls simultaneously a standard function
   and an mthread function. */

#include <stddef.h>
#include <pthread.h>
#include "mthread_queue.h"
void** q;
pthread_t        th1, th2;
int random(void);
void *t (void *_) {return NULL;}

int f1(void * p, int i) {
  pthread_create( &th2, NULL, t, NULL);
  return 0;
}

int f2(void * p, int i) {
  pthread_create( &th1, NULL, t, NULL);
  return 0;
}

void main () {
  int (*p)(void *, int);

  if (random ())
    p = &f1;
  else
    p = &queuecreate;

  (*p)(&q,1);

  if (random ())
    p = &f2;
  else
    p = &Frama_C_queue_init;

  (*p)(&q,1);

}
