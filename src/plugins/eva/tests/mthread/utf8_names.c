/* run.config
   ENABLED_IF: %{bin-available:dot}
   LOG: @LOG_MT_DOT_FILES_FILENAME@
   STDOPT: +"-eva-verbose 0 -mt-verbose 1 @LOG_MT_DOT_FILES_OPTS@"
*/
/* run.config*
   COMMENT: Deactivate alternative configurations.
   DONTRUN:
*/

#include <stddef.h>
#include <string.h>
#include <string.c>
#include <mthread.h>

#define BUF_SIZE 20


__fc_mthread_id job1, job2, mutex, queue;
int a, b, c;

int g(int *from) {
  return *from+"\"𐍅\" /0";
}

void * f1(void * _) {
  a = g(&b);
  Frama_C_mutex_lock(mutex);
  c = 1;
  Frama_C_mutex_unlock(mutex);
  char buf[BUF_SIZE] = { 0 };
  strcpy(buf, "\"𐍅\" /1");
  Frama_C_queue_send(queue, buf, BUF_SIZE);
  return NULL;
}
void * f2(void * _) {
  b = g(&a);
  Frama_C_mutex_lock(mutex);
  c = 2;
  Frama_C_mutex_unlock(mutex);
  char buf[BUF_SIZE] = { 0 };
  strcpy(buf, "\"𐍅\" /2");
  Frama_C_queue_send(queue, buf, BUF_SIZE);
  return NULL;
}

void main() {
  // All names use a slash, a double-quote and the multibyte UTF-8
  // character 𐍅 (U+10345)
  mutex = Frama_C_mutex_init("mutex / \"𐍅\"");
  queue = Frama_C_queue_init("queue / \"𐍅\"", 2);

  job1 = Frama_C_thread_create("job / \"𐍅\" f1", &f1, NULL);
  Frama_C_thread_start(job1);
  job2 = Frama_C_thread_create("job / \"𐍅\" f2", &f2, NULL);
  Frama_C_thread_start(job2);

  char buf[BUF_SIZE] = { 0 };
  while (c == 0) {
    Frama_C_queue_receive(queue, BUF_SIZE, buf);
  }
}
