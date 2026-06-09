/* run.config
  EXIT: 1
  EXECNOW: BIN @PTEST_NAME@.sav.error LOG @PTEST_NAME@_sav.res LOG @PTEST_NAME@_sav.err @frama-c@ -eva @EVA_OPTIONS@ -eva-stop-at-nth-alarm 4 @PTEST_FILE@ -save @PTEST_NAME@.sav > @PTEST_NAME@_sav.res 2> @PTEST_NAME@_sav.err
  EXIT: 0
  PLUGIN: @PTEST_PLUGIN@ report
  OPT: -load %{dep:@PTEST_NAME@.sav.error} -out -input -deps -report
*/
/* run.config*
   DONTRUN: avoids duplication of oracles
*/

/* Tests the "clean" stop of the analysis (here via -eva-stop-at-nth-alarm)
   with -save name: save a valid session file "name.error", which can then be
   reloaded and contains partial results of the functions analyzed before the
   crash. */

volatile unsigned int nondet;

int t[10];

int g;

/* Completely analyzed: results should be correct. */
void init (int x) {
  int i = 0;
  //@ loop unroll 10;
  for (; i < 10; i++) {
    t[i] = nondet % x;  // alarm: modulo by zero
  }
  g = t[x];             // alarm: out of bound index
  if (nondet) g = t[i]; // invalid alarm: out of bound index
  //@ assert valid: i == 10;
}

/* Partially analyzed, so partial results… */
void partial (int x) {
  //@ assert valid: 0 <= x;
  g = 100 / x; // division by zero
  // The analysis stops here. Alarms below are never emitted.
  g = nondet + x; // overflow alarm
  g = t[x];       // out of bounds alarms
  //@ assert unreached: g > 0;
}

/* Unreached by the analysis, as it stops before. */
void unreached (int x) {
  g = x / x;
  //@ assert unreached: g == 1;
}

void main (void) {
  init(nondet % 100);
  partial(nondet % 100);
  unreached(nondet % 100);
}
