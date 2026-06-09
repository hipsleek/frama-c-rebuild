/* run.config*
 EXIT: 1
   STDOPT: +"-mt-write-races"
*/
/* This file tests a major degeneration during the value analysis, where
   the whole memory is accessed. */
#include <stddef.h>
#include <pthread.h>

pthread_t        jobs;
int random(void);

int *p, r;

/*@ assigns *p, r; */
int g(void);

void *f (void* p) {
  g ();
  return NULL;
}

void main () {

  p = random();

  pthread_create(&jobs , NULL, f, p);

  r = g();
}
