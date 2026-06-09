/* run.config
   EXIT: 1
   STDOPT: -wp -wp-prover qed
 */

/* run.config_qualif
   DONTRUN:
 */

int main_called = 0;

/*@ ensures main_called == 0; */
int main() {
    if (main_called) { /*@ assert \false; */ }
    else { main_called = 1; return main(); }
}
