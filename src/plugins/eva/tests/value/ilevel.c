/* run.config*
   PLUGIN: @EVA_MAIN_PLUGINS@ inout,slicing
   OPT: -eva @EVA_CONFIG@ -slice-return main -then-on "Slicing export" -eva -eva-ilevel 16 -eva-msg-key=progress -then-on "default" -eva-ilevel 17 -then -eva-ilevel 48
   STDOPT: +"-eva-ilevel 400 -main large_ilevel"
*/

// Test in particular that ilevel is by-project, even though it is an ocaml ref

#include "__fc_builtin.h"

volatile int nondet;
int i, j, k, l;

int main () {
  do { i = nondet; }
  while (! (0 <= i && i < 8));

  do { j = nondet; }
  while (! (0 <= j && j < 17));

  k = j;
  if (k == 16) k = 15;

  l = nondet;
  if (nondet) {
    //@ assert 0 <= l <= 4;
  } else {
    //@ assert 6 <= l <= 9;
  }
  Frama_C_show_each(l); // Possible problem with cache on offsetmap join

  return i+j+k+l;
}

/* Tests printing large integer sets. */
void large_ilevel (void) {
  int i = Frama_C_interval(0, 10);
  int j = nondet ? i : i - 25;
  int k = Frama_C_interval(100, 200);
  if (k == 128) return;

  int a[11] = {53, 17, 64, 99, 25, 12, 72, 81, 404, 303, -101};

  int s2 = nondet ? 40 : (nondet ? 41 : 42);
  int s1 = a[i];

  int x = nondet ? (nondet ? j : k) : (nondet ? s1 : s2);
}
