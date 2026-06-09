/* run.config
   MACRO: SIGNAL_CONF -eva-split-return-function signal:full -eva-slevel-function signal:2
   MACRO: RAISE_CONF -eva-split-return-function raise:auto -eva-slevel-function raise:2
   STDOPT: #"-eva-slevel 2 @SIGNAL_CONF@ @RAISE_CONF@"
   STDOPT: #"-eva-slevel 2 -cpp-extra-args=-DWITH_SIGNAL_C @SIGNAL_CONF@ @RAISE_CONF@"
*/
/* The interaction between signal() and raise() can only be exploited by Eva if
   signal.c is included and the splits inside those functions are propagated to
   the callers. The macros SIGNAL_CONF and RAISE_CONF add -eva-split-return and
   -eva-slevel for those two functions to do just that. */
#include <errno.h>
#include <signal.h>
#include <stdint.h> // for SIG_ATOMIC_MIN/MAX
#ifdef WITH_SIGNAL_C
  #include <signal.c>
#endif

volatile int nondet;

int test_sigaction() {
  sigset_t s;
  if (sigemptyset(&s)) return 1;
  if (sigaddset(&s, SIGALRM)) {
    return -1;
  }
  if (sigdelset(&s, SIGUSR1)) {
    return -1;
  }
  if (!sigismember(&s, SIGALRM)) return 2;
  sigfillset(&s);
  if (!sigismember(&s, SIGPIPE)) return 3;
  sigset_t uninit;
  if (nondet) {
    if (sigaddset(&uninit, SIGKILL)) {
      return -1;
    }
    //@ assert unreachable_if_precise: \false;
  }

  sigset_t old;
  if (sigprocmask(SIG_SETMASK, 0, &old)) {
    return -1;
  }
  if (sigaddset(&old, SIGALRM)) {
    return -1;
  }
  if (sigprocmask(SIG_SETMASK, &old, 0)) {
    return -1;
  }
  if (sigprocmask(SIG_BLOCK, &s, &old)) {
    return -1;
  }

  int kill_res = kill(42, SIGTERM);

  struct sigaction sa1, sa2;
  if (sigaction(SIGCHLD, 0, &sa1)) {
    return -1;
  }
  if (sigaction(SIGCONT, &sa1, &sa2)) {
    return -1;
  }
  if (sigaction(SIGUSR1, &sa2, 0)) {
    return -1;
  }

  //@ assert valid_nsig: NSIG >= 0;

  if (nondet) {
    errno = 0;
    int r = sigsuspend(&s);
    //@ assert sigsuspend_errno_eintr: errno == EINTR;
    //@ assert sigsuspend_return: r == -1;
  }

  return 0;
}

int signal_status;

void signal_handler(int signal)
{
  signal_status = signal;
}

void test_sighandler() {
  //@ assert signal_status == 0;

  sighandler_t res_sig = signal(SIGINT, signal_handler);
  //@ split res_sig != SIG_ERR;

  int res_raise = raise(SIGINT);
  //@ split res_raise < 0;
  //@ split res_raise > 0;

  // First check will fail on one of the partition if `signal.c` is not included
  // as Eva cannot know that `signal_handler` has been called.
  //@ ghost int handler_called = res_sig != SIG_ERR && res_raise == 0;
  //@ assert handler_called: handler_called ==> signal_status == SIGINT;
  //@ assert handler_not_called: !handler_called ==> signal_status == 0;
}

int main() {
  test_sigaction();
  test_sighandler();
  sig_atomic_t smin = SIG_ATOMIC_MIN, smax = SIG_ATOMIC_MAX;
  //@ assert unknown_because_volatile: smin <= smax;
  return 0;
}
