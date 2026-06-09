/* run.config*
   ENABLED_IF: %{bin-available:dot}
   LOG: @LOG_MT_DOT_FILES_FILENAME@
   STDOPT: +"-mt-non-shared-accesses @LOG_MT_DOT_FILES_OPTS@"
*/
/* This file is used to give an example of a cfg with many features */

#include <stddef.h>
#include <pthread.h>
#include <mthread.h>

volatile int nondet;
int random(void);

pthread_t  jobs[4];
int x, global1, global2[2];

void *fjob(void *_) {
  int r = global1 + global2[0] + global2[1];
  return NULL;
}

void g1(int* v, int i) {
  if (i<4)
    pthread_create(&jobs[i], NULL, fjob, NULL );
  else
    *v = 1;
}

void g2(int* v) {
  if (random())
    *v = 1;
  else
    *(v+1) = 2;
}

int a, b, c, d, e, f;
pthread_mutex_t lock;

/*@ assigns a \from b; */
void g3(void);

/*@ assigns b \from a; */
void g4(void);

/*@ assigns *to \from *from; */
void g5(int * to, int * from);

int g6(int * from) {
  return *from;
}

void *f1(void * _) {
  pthread_mutex_lock(&lock);
  g3();
  Frama_C_mthread_show("custom event", x, a, b);
  pthread_mutex_unlock(&lock);
  g5(&c, &d);
  e = g6(&f);
  return NULL;
}

void *f2(void * _) {
  pthread_mutex_lock(&lock);
  g4();
  Frama_C_mthread_show("other custom event", x, a, b);
  pthread_mutex_unlock(&lock);
  g5(&d, &c);
  f = g6(&e);
  return NULL;
}

void main() {
  if (nondet) {
    int i, arr[2];
    void (*pf)(int*, int) = &g1;

    g1(NULL, 0);
    g2(arr);
    for (i=1;i<5;i++)
      if (!x) {
        (*pf)(&global1, i);
        g2(global2);
      }
  } else {
    pthread_t job_f1, job_f2;
    a = 1; b = 1; c = 1; d = 1; e = 1; f = 1;
    pthread_mutex_init(&lock, NULL);
    pthread_create(&job_f1, NULL, f1, NULL);
    pthread_create(&job_f2, NULL, f2, NULL);
    int sum = 0;
    pthread_mutex_lock(&lock);
    sum += a;
    sum += b;
    pthread_mutex_unlock(&lock);
    sum += c;
    sum += d;
    sum += e;
    sum += f;
    Frama_C_mthread_show("main event without argument");
    // Incorrect use of builtin Frama_C_mthread_show.
    char event[4] = {0};
    Frama_C_mthread_show(event);
  }
}
