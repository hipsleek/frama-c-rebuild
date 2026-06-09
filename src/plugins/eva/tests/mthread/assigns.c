/* Test of shared vars for functions with ACSL assigns specification */

#include <stddef.h>
#include <pthread.h>

pthread_t        jobs1;
pthread_t        jobs2;

int a;
int b;


/*@ assigns a \from b; */
void g1(void);

/*@ assigns b \from a; */
void g2(void);

void *f1(void *_) {
  g1();
  return NULL;
}

void *f2(void *_) {
  g2();
  return NULL;
}

void main() {
  pthread_create( &jobs1 , NULL, f1, NULL);
  pthread_create( &jobs2 , NULL, f2, NULL);
}
