/*  run.config*
    COMMENT: Test pointer-induced implicit flows.
*/

/* This is based on the example in Listing 3.4 (Section 3.2.5, p. 29) in
 * Mounir's thesis. */

extern int user_input;
extern int __fc_private secret;

int x, y, z;
int *p;

int main(void) {
    if (user_input == secret) {
        p = &y;
    } else {
        p = &z;
    }
    x = 1;
    *p = 1;

    /* x is assigned a constant in a non-secret context, thus it must be
     * public. */
    /*@ assert security_status(x) == public; */

    /* *p has an implicit flow from the secret, as does p. */
    /*@ assert security_status(p) == private; */
    /*@ assert security_status(*p) == private; */

    /* y and z have implicit flows from the secret: They are assigned in a
     * secret context. Regardless of which branch above is executed, both
     * variables must be treated as secret. */
    /*@ assert security_status(y) == private; */
    /*@ assert security_status(z) == private; */

    return 0;
}
