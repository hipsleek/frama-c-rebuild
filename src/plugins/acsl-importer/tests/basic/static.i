/* run.config
PLUGIN: @PTEST_PLUGIN@ eva,scope
STDOPT: -acsl-import %{dep:./@PTEST_NAME@.acsl} -then -eva
PLUGIN: acsl-importer
STDOPT: -acsl-import %{dep:./@PTEST_NAME@.acsl} -then -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
*/

int counter = 0;

void incr(int i) {
  static int counter = 0;
  counter += i;
}

void decr(int i) {
  static int counter = 0;
  counter -= i;
}

volatile int c;

int main () {
  incr(4);
  decr(5);
}
