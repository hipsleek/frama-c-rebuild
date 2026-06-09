/* run.config*
   ENABLED_IF: %{bin-available:dot}
   LOG: @LOG_MT_DOT_FILES_FILENAME@
   STDOPT: +"-mt-non-shared-accesses @LOG_MT_DOT_FILES_OPTS@"
*/

#include <stddef.h>
#include <pthread.h>

struct pair {
  int data;
  int data2;
};

struct pair s1, s2;

pthread_t        jobs;
pthread_mutex_t  lock1, lock2;

void *f(void *_) {
  pthread_mutex_lock(&lock1);
  s1.data++;
  s1.data2 += 3;
  pthread_mutex_unlock(&lock1);
  return NULL;
}

void *g(void *_) {
  pthread_mutex_lock(&lock2);
  s2.data++;
  s2.data2 += 3;
  pthread_mutex_unlock(&lock2);
}

volatile int nondet;
int main()
{
  pthread_mutex_init(&lock1, NULL);
  pthread_mutex_init(&lock2, NULL);
  for (int i = 0; i < 4; ++i) {
    if (nondet) {
      pthread_create(&jobs , NULL, f, NULL);
    } else {
      pthread_create(&jobs, NULL, g, NULL);
    }
  }

  int t=0;
  pthread_mutex_lock(&lock1);
  t += s1.data;
  t += s1.data2;
  pthread_mutex_unlock(&lock1);
  pthread_mutex_lock(&lock2);
  t += s2.data;
  t += s2.data2;
  pthread_mutex_unlock(&lock2);
  return t;
}
