/* run.config*
   STDOPT: -eva-partition-history 2 -eva-slevel 0
   STDOPT: -eva-partition-history 2 -eva-slevel 10
   STDOPT: -eva-slevel 10 -eva-split-return full -main check_slevel
*/

/* Tests the order in which list of states are propagated via the different
   state partitioning techniques during an Eva analysis.
   This order has no impact on the soundness or the precision of the analysis.
   However, ideally, we would like this order to be stable during the analysis,
   in order to make its behavior more easily understandable.

   In each function check_* below, the order of states shown by successive
   Frama_C_show_each directives should ideally remain stable. */

volatile unsigned int nondet;

int g = 0;

void incr_g (void) {
  g++;
}

void check_split_order (void) {
  int x = nondet % 3;
  //@ split x;
  Frama_C_show_each_split(x);
  Frama_C_show_each_split(x);
  g++;
  Frama_C_show_each_split(x);
  incr_g();
  Frama_C_show_each_split(x);
  //@ check g > 0;
  Frama_C_show_each_split(x);
  for (int j = 0; j < 100; j++) ;
  Frama_C_show_each_split(x);
}

void check_history_order (void) {
  int x = 0;
  if (nondet) x = 1;
  if (nondet) x = 2;
  Frama_C_show_each_history(x);
  Frama_C_show_each_history(x);
  g++;
  Frama_C_show_each_history(x);
  incr_g();
  Frama_C_show_each_history(x);
  //@ check g > 0;
  Frama_C_show_each_history(x);
  for (int j = 0; j < 100; j++) ;
  Frama_C_show_each_history(x);
}

/* Test non-slevel partitioning, either with or without slevel. */
void main (void) {
  check_split_order();
  check_history_order();
}

/* Tests slevel partitioning. */
int check_slevel_order (void) {
  int x = 0;
  if (nondet) x = 1;
  if (nondet) x = 2;
  Frama_C_show_each_slevel(x);
  Frama_C_show_each_slevel(x);
  g++;
  Frama_C_show_each_slevel(x);
  incr_g();
  Frama_C_show_each_slevel(x);
  //@ check g > 0;
  Frama_C_show_each_slevel(x);
  return x;
}

/* Tests slevel partitioning with -eva-split-return. */
void check_slevel (void) {
  int a = check_slevel_order();
  Frama_C_show_each(a);
}
