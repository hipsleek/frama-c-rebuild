/* run.config*
   STDOPT: +"-mt-interrupt-handlers interrupt"
   STDOPT: +"-main main_ptr_write"
   STDOPT: +"-main main_thread_loop"
*/

#include <stddef.h>
#include <pthread.h>

volatile int nondet;
int shared_a, shared_b, shared_c;

void interrupt(void) {
  Frama_C_show_each_interrupt_start(shared_a, shared_b, shared_c);
  shared_b = 1;
  Frama_C_show_each_interrupt_end(shared_a, shared_b, shared_c);
}

void *fjob1(void * _) {
  Frama_C_show_each_fjob1_start(shared_a, shared_b, shared_c);
  shared_b = 2;
  shared_c = 1;
  Frama_C_show_each_fjob1_end(shared_a, shared_b, shared_c);
  return NULL;
}

void *fjob2(void * _) {
  Frama_C_show_each_fjob2_start(shared_a, shared_b, shared_c);
  shared_a = 3;
  int t = shared_c;
  Frama_C_show_each_fjob2_end(shared_a, shared_b, shared_c);
  return NULL;
}

void *fjob3(void * _) {
  Frama_C_show_each_fjob3_start(shared_a, shared_b, shared_c);
  shared_a = 2;
  Frama_C_show_each_fjob3_end(shared_a, shared_b, shared_c);
  return NULL;
}

void print(int a, int b, int c) {
  // Expected values:
  // - {1, 2, 3} for a and shared_a
  // - {0, 1, 2} for b and shared_b
  // - {0, 1} for c and shared_c
  Frama_C_show_each_a(a);
  Frama_C_show_each_b(b);
  Frama_C_show_each_c(c);
  Frama_C_show_each_shared_a(shared_a);
  Frama_C_show_each_shared_b(shared_b);
  Frama_C_show_each_shared_c(shared_c);
}

void main() {
  Frama_C_show_each_main_start(shared_a, shared_b, shared_c);
  pthread_t job1, job2, job3;

  pthread_create(&job1, NULL, fjob1, NULL);

  shared_a = 1;

  pthread_create(&job2, NULL, fjob2, NULL);
  pthread_create(&job3, NULL, fjob3, NULL);

  print(shared_a, shared_b, shared_c);
  Frama_C_show_each_main_end(shared_a, shared_b, shared_c);
}

void write(int * dest, int value) {
  *dest = value;
}

void * fjobptr1(void * _) {
  Frama_C_show_each_fjobptr1_start(shared_a, shared_b);
  int t = shared_a + shared_b;
  if (nondet) {
    write(&shared_a, 1);
  } else {
    write(&shared_b, 1);
  }
  Frama_C_show_each_fjobptr1_end(shared_a, shared_b);
  return NULL;
}

void * fjobptr2(void * _) {
  Frama_C_show_each_fjobptr2_start(shared_a, shared_b);
  int t = shared_a + shared_b;
  if (nondet) {
    write(&shared_b, 2);
  } else {
    write(&shared_a, 2);
  }
  Frama_C_show_each_fjobptr2_end(shared_a, shared_b);
  return NULL;
}

void main_ptr_write(void) {
  Frama_C_show_each_main_start(shared_a, shared_b);
  pthread_t jobptr1, jobptr2;
  pthread_create(&jobptr1, NULL, fjobptr1, NULL);
  pthread_create(&jobptr2, NULL, fjobptr2, NULL);
  // Expected values:
  // - {0, 1, 2} for shared_a
  // - {0, 1, 2} for shared_b
  Frama_C_show_each_main_end(shared_a, shared_b);
}

void * fjoboneshot(void * _) {
  Frama_C_show_each_fjoboneshot_start(shared_a, shared_b);
  int t = shared_a + shared_b;
  shared_a++;
  Frama_C_show_each_fjoboneshot_end(shared_a, shared_b);
  return NULL;
}

void * fjoblooped(void * _) {
  Frama_C_show_each_fjoblooped_start(shared_a, shared_b);
  int t = shared_a + shared_b;
  shared_b++;
  Frama_C_show_each_fjoblooped_end(shared_a, shared_b);
  return NULL;
}

void main_thread_loop(void) {
  Frama_C_show_each_main_start(shared_a, shared_b);
  pthread_t joboneshot, joblooped;
  pthread_create(&joboneshot, NULL, fjoboneshot, NULL);
  while (nondet) {
    pthread_create(&joblooped, NULL, fjoblooped, NULL);
  }
  // Expected values:
  // - {0, 1} for shared_a as the thread is only run once
  // - [0..MAX_INT] for shared_b as the thread is created in a loop
  Frama_C_show_each_main_end(shared_a, shared_b);
}
