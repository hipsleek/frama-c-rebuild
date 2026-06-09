/* run.config*
   ENABLED_IF: %{bin-available:dot}
   LOG: @LOG_MT_DOT_FILES_FILENAME@
   STDOPT: #"@PTEST_SHARE_DIR@/mthread/mthread_queue.c" +"@LOG_MT_DOT_FILES_OPTS@"
*/
/*
  THIS FILE IS USED AS AN EXAMPLE FOR THE WEBSITE. DO NOT FORGET TO UPDATE THE
  EXAMPLES ON THE WEBSITE IF IT IS EDITED.
  When updating the examples, copy the file in a new folder, remove comments
  until the ---- line and then run the following command to generate the log
  file and the HTML summary:

  frama-c -mthread -mt-threads-lib pthreads -mt-shared-values 2 \
    -mt-shared-accesses-synchronization \
    $(frama-c -print-share-path)/mt/mthread_queue.c \
    -eva-verbose 0 -mt-extract html \
    -eva-slevel 15 philo.c > output.txt
*/
/* -------------------------------------------------------------------------- */
/* All-purpose example, implementing a slightly complexified version of the
   dining philosphers problems */

#include <stddef.h>
#include <pthread.h>
#include "mthread_queue.h"
#define N 5


int end2 = 0;
pthread_mutex_t  locks[N];
pthread_t        jobs[N];
msgqueue_t queue;


int random(void);

void aux (int l, int r, int mess) {
  pthread_mutex_lock(locks+l);
  pthread_mutex_lock(locks+r);
  if (random() && mess != 2) {
    char buf[2];
    buf[0]=mess;
    end2 = 1;
    msgsnd(queue, buf, 2);
  }
  pthread_mutex_unlock(locks+r);
  pthread_mutex_unlock(locks+l);
}

void * job( void * k ) {
  int p = (int) k ;
  int l = p>0 ? p-1 : N-1 ;
  int r = p<N-1 ? p+1 : 0 ;

  while(1)
    aux(l, r, p+1);
}

int main() {
  int i ;
  char end[2];
  end[0]=0;

  for(i=0;i<N;i++)
    pthread_mutex_init( &locks[i] , NULL);

  queuecreate(&queue, 5);

  for(i=0;i<N;i++)
    pthread_create( &jobs[i], NULL, job, (void *) i );

  while(!(end[0] && __MTHREAD_SYNC(end2)))
    msgrcv(queue, 2, end);

  return 0;
}
