/* run.config
   COMMENT: Test signal handlers
*/

#include <signal.h>
#include <stdio.h>

typedef void (*sighandler_t)(int);

volatile sig_atomic_t signal_status;

void signal_handler(int signal) {
  signal_status = signal;
}

void test_sighandler() {
  //@ assert signal_status == 0;

  sighandler_t res_sig = signal(SIGINT, signal_handler);
  //@ assert ok_signal: res_sig != SIG_ERR;

  int res_raise = raise(SIGINT);
  //@ assert ok_raise: res_raise == 0;

  //@ assert status_changed: signal_status == SIGINT;
}

int main() {
  test_sighandler();
  return 0;
}
