/*  run.config*
    COMMENT: Test implicit flows due to loops.
    STDOPT: +"-eva-slevel 1"
*/

extern unsigned int init;
extern unsigned int __fc_private secret;

int main(void) {
    unsigned int sum = init;
    unsigned int iterations = 0;
    /*@ assert security_status(sum) == public; */
    /*@ assert security_status(iterations) == public; */
    while (secret > 0u) {
        sum += secret;
        iterations += 1u;
        /*@ assert security_status(iterations) == private; */
        secret -= 1u;
    }
    // The private label must flow from secret to these variables even if
    // the loop is never executed.
    /*@ assert security_status(sum) == private; */
    /*@ assert security_status(iterations) == private; */

    /* The same thing once again with a do-while loop. */
    sum = init;
    iterations = 0;
    /*@ assert security_status(sum) == public; */
    /*@ assert security_status(iterations) == public; */
    do {
        sum += secret;
        iterations += 1u;
        // We can *not* assert the following because it does not, in fact,
        // hold on the first iteration! The loop's program counter is only
        // updated after the first time the body is executed.
        /* @ assert security_status(iterations) == private; */
        // The following holds, as pointed out by Julien.
        /*@ assert iterations > 1 ==> security_status(iterations) == private;
         */
        secret -= 1u;
    } while (secret > 0u);
    // Regardless of the comment above, these assertions hold because at
    // this point the program counter's label has definitely be updated and
    // definitely been used to update all of the variables modified in the
    // loop.
    /*@ assert security_status(sum) == private; */
    /*@ assert security_status(iterations) == private; */

    return sum;
}
