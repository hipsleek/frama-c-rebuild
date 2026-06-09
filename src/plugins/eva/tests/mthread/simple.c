/* run.config*
   ENABLED_IF: %{bin-available:dot}
   STDOPT: +"-mt-stop-after 1"
   LOG: @LOG_MT_DOT_FILES_FILENAME@
   STDOPT: +"-mt-full-cfg @LOG_MT_DOT_FILES_OPTS@"
*/

#include <stddef.h>
#include <pthread.h>

int a, b, c, d, e, v;
pthread_t job1, job2;
pthread_mutex_t  lock;

//@ assigns v \from \nothing; ensures v == 1;
void g1(void); // { v = 1; }

//@ assigns v \from \nothing; ensures v == 2;
void g2(void);

void *f1(void * p) {
  pthread_mutex_lock(&lock);
  a = 1;
  *((int*)p) = 1;
  c = 1;
  g1();
  pthread_mutex_unlock(&lock);
  e = 1;
  return NULL;
}

void *f2(void * p) {
  pthread_mutex_lock(&lock);
  b = 2;
  *((int*)p) = 2;
  c = 2;
  g2();
  pthread_mutex_unlock(&lock);
  d = 2;
  return NULL;
}

int main() {
  pthread_mutex_init( &lock, NULL);

  pthread_create(&job1, NULL, f1, (void *) &b);
  pthread_create(&job2, NULL, f2, (void *) &a);

  pthread_mutex_lock(&lock);
  int r = a+b+c+v;
  pthread_mutex_unlock(&lock);
  return r;
}
