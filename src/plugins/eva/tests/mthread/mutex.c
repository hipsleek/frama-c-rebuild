/* This file tests the detection of mutexes that are taken when a global
   variable is accessed */

#include <stddef.h>
#include <pthread.h>

int s1 = 0;
int s2 = 0;
int s3 = 0;

pthread_t        jobs1;
pthread_t        jobs2;
pthread_t        jobs3;
pthread_t        jobs4;

pthread_mutex_t  m1;
pthread_mutex_t  m2;


int random(void);

void *f1(void *_) {
  pthread_mutex_lock(&m1);
  pthread_mutex_lock(&m2);
  s1 = 1;
  pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m1);
  return NULL;
}

void *f2(void *_) {
  pthread_mutex_lock(&m1);
  int v1 = s1;
  int v2 = s2;
  pthread_mutex_unlock(&m1);
  return NULL;
}

void *f3(void *_) {
  s2 = 3;
  return NULL;
}


void *f4(void *_) {
  return NULL;
}

int main(void)
{
  pthread_mutex_init( &m1 , NULL );
  pthread_mutex_init( &m2 , NULL );

  pthread_create( &jobs1 , NULL, f1, NULL);
  pthread_create( &jobs2 , NULL, f2, NULL);
  pthread_create( &jobs3 , NULL, f3, NULL);
  pthread_create( &jobs4 , NULL, f4, NULL);

  return 0;
}
