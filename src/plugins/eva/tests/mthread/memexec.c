#include <stddef.h>
#include <pthread.h>

volatile int v;
int a, c;

pthread_t        th1;

void g() {
  c = 2;
}

void f() {
  a = 1;
  if (v)
    g();
  else {
    g();
    g();
  }
}

void *t1 (void* _) {
  if (v)
    f();
  else
    f();
  return NULL;
}

int main () {
  pthread_create( &th1, NULL, t1, NULL);
  int b = a+c;
}
