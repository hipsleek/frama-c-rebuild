/*  run.config*
    COMMENT: Test implicit flows due to simple branching (more complex flows).
*/

extern int user_input;
extern int another_user_input;
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

    /* Slight variation of the above: Assign z in both branches. Expect an
     * output only updating z's label once per branch, i.e., don't add a
     * redundant label update for a variable that's explicitly updated
     * anyway. */
    y = 0;
    z = 0;
    /*@ assert security_status(y) == public; */
    /*@ assert security_status(z) == public; */
    if (user_input == secret) {
        y = 2;
        z = 2;
    } else {
        z = 3;
    }
    /*@ assert security_status(y) == private; */
    /*@ assert security_status(z) == private; */

    y = 0;
    /*@ assert security_status(y) == public; */
    /* Yet another variation: No explicit else branch. We still expect an
     * else branch with a label update in the transformed program. */
    if (user_input == secret) {
        y = 4;
    }
    /*@ assert security_status(y) == private; */

    y = 0;
    /*@ assert security_status(y) == public; */
    z = 0;
    /*@ assert security_status(z) == public; */
    /* One more: Nested ifs. */
    if (user_input == secret) {
        if (another_user_input != secret) {
            /* new secret context */
            z = 5;
        }
        /*@ assert security_status(z) == private; */
        if (another_user_input != user_input) {
            /* new context that inherits its secrecy from the enclosing
             * context */
            y = 6;
        }
        /*@ assert security_status(y) == private; */
    }
    /*@ assert security_status(y) == private; */
    /*@ assert security_status(z) == private; */

    /* For completeness: The status of x and the user inputs hasn't changed.
     */
    /*@ assert security_status(x) == public; */
    /*@ assert security_status(user_input) == public; */
    /*@ assert security_status(another_user_input) == public; */

    return 0;
}
