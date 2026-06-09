/* run.config*
DEPS: preprocess_dos.sh
ENABLED_IF: %{bin-available:unix2dos}
OPT: -cpp-command="./@PTEST_NAME@.sh unix2dos %i %o" -cpp-frama-c-compliant -print
*/

int main() {
    int a = 0;
    /*@
        assert a == 0;
    */
    return a;
}
