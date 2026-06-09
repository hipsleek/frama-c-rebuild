#include <stddef.h>
#include <pthread.h>

struct s0 {
  pthread_mutex_t a0;
  pthread_mutex_t b0;
  pthread_mutex_t c0;
};

typedef struct {
  pthread_mutex_t a1;
  struct s0 b1[5];
  pthread_mutex_t c1;
} s1;

s1 t[3];

void main() {
  pthread_mutex_init(&t[0].a1 , NULL);
  pthread_mutex_init(&t[1].b1[2].a0 , NULL);
  pthread_mutex_init(&t[1].b1[4].b0 , NULL);
  pthread_mutex_init(&t[2].c1 , NULL);
}
