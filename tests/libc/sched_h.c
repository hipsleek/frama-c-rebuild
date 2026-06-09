#include <sched.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
  int r;
  r = sched_get_priority_max(SCHED_OTHER);
  r = sched_get_priority_max(SCHED_RR);
  struct sched_param sp;
  r = sched_getparam(0, &sp);
  struct timespec interval;
  r = sched_rr_get_interval(0, &interval);
  r = sched_setscheduler(1, SCHED_FIFO, &sp);
  return 0;
}
