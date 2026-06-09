/* run.config*
  STDOPT: +"-main main_interrupt_one_shot -mt-interrupt-handlers interrupt_oneshot -mt-threads-lib builtins-only"
  STDOPT: +"-main main_interrupt_incr -mt-interrupt-handlers interrupt_incr -mt-threads-lib builtins-only"
  STDOPT: +"-main main_interrupt_cross_incr -mt-interrupt-handlers interrupt_cross_incr1,interrupt_cross_incr2 -mt-threads-lib builtins-only"
*/
int interrupt_received;

void interrupt_oneshot(void) {
  interrupt_received = 1;
}

void do_something(void) {
  // anything
}

int main_interrupt_one_shot() {
  while (1) {
    Frama_C_show_each_locked(interrupt_received);
    if (interrupt_received) {
      Frama_C_show_each(interrupt_received);
      interrupt_received = 0;
      Frama_C_show_each(interrupt_received);
      do_something();
    }
  }
}

void interrupt_incr(void) {
  interrupt_received++;
}

int main_interrupt_incr() {
  Frama_C_show_each(interrupt_received);
  return 0;
}

int interrupt_received_a;
int interrupt_received_b;

void interrupt_cross_incr1(void) {
  interrupt_received_a = interrupt_received_b + 1;
}

void interrupt_cross_incr2(void) {
  interrupt_received_b = interrupt_received_a + 1;
}

void print_a(int a) {
  Frama_C_show_each_a(a);
}
void print_b(int b) {
  Frama_C_show_each_b(b);
}

int main_interrupt_cross_incr() {
  // Check that the interferences are correctly passed through function
  // parameters, even when the main function does not explicitly start threads.
  print_a(interrupt_received_a);
  print_b(interrupt_received_b);
  return 0;
}
