/*  run.config*
    COMMENT: Test implicit flows due to simple branching.
*/

/* This is essentially the example from Listing 3.1 (Section 3.2.5, p. 27)
 * from Mounir's thesis. */

extern int user_input;
extern int __fc_private secret;

int x, y, z;

int main(void) {
    if (user_input == secret) {
        y = 1;
        /*@ assert security_status(y) == private; */
    } else {
        z = 1;
        /*@ assert security_status(z) == private; */
    }
    x = 1;

    /* x is assigned a constant in a non-secret context, thus it must be
     * public. */
    /*@ assert security_status(x) == public; */

    /* y and z have implicit flows from the secret: They are assigned in a
     * secret context. Regardless of which branch above is executed, both
     * variables must be treated as secret. */
    /*@ assert security_status(y) == private; */
    /*@ assert security_status(z) == private; */

    return 0;
}
