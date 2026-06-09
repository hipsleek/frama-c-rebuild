/* This example tests concurrent accesses to different array cells */

#include <stddef.h>
#include <pthread.h>
#define N 5

char t[5];

char tt[26];

pthread_t        jobs1;
pthread_t        jobs2;
pthread_t        jobs3;
pthread_t        jobs4;
pthread_t        jobs5;
pthread_t        jobs6;


int random(void);

void *f1(void *_) {
  t[1]=11;
  t[2]=12;
  return NULL;
}

void *f2(void *_) {
  t[1]=21;
  return NULL;
}

void *f3(void *_) {
  *((int*)(&t[0])) = 0x01234567;
  return NULL;
}

void *f4(void *_) {
  for (int i=0;i<=25;i++)
    tt[i]=(i+1);
  return NULL;
}

void *f5(void *_) {
  for (int i=0;i<=25;i++)
    tt[i]=(i+2);
  return NULL;
}

void *f6(void *_) {
  return NULL;
}

void main(void)
{
  pthread_create( &jobs1 , NULL, f1, NULL);
  pthread_create( &jobs2 , NULL, f2, NULL);
  pthread_create( &jobs3 , NULL, f3, NULL);
  pthread_create( &jobs4 , NULL, f4, NULL);
  pthread_create( &jobs5 , NULL, f5, NULL);
  pthread_create( &jobs6 , NULL, f6, NULL);
}
