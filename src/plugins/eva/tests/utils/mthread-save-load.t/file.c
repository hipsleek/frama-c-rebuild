#include <stddef.h>
#include <pthread.h>

int x, y;
pthread_t job1, job2;

void *f(void *p) {
  x = y + 1;
  return NULL;
}

void *g(void *p) {
  y = x + 1;
  return NULL;
}

int main() {
  pthread_create(&job1, NULL, f, NULL);
  pthread_create(&job2, NULL, g, NULL);
  return 0;
}
