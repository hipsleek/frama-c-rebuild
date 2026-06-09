/*  run.config*
    COMMENT: Test aspects of the initialization of label variables.
*/

/* If global declarations are annotated, expect initialization of their
 * status variables even if the variables themselves are extern. */
extern int __fc_private a;
extern int b;

int main(void) {
    /* Evaluation of this expression generates complex control flow and
     * introduces temporary variables. Make sure their security labels are
     * inserted and updated correctly. */
    int c = (--a < b || (a++ && a < b));
    /*@ assert security_status(c) == private; */

    return c;
}
