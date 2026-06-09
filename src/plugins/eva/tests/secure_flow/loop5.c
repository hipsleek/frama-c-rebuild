/*  run.config*
    COMMENT: Test implicit flows due to continue in a switch in a loop.
    STDOPT: +"-eva-slevel 4"
*/

extern unsigned int user_input;
extern unsigned int __fc_private secret;

int main(void) {
    int x, y, loop_was_entered, after_first_iteration;

    x = user_input;
    loop_was_entered = 0;
    after_first_iteration = 0;
    y = 0;
    /*@ assert security_status(y) == public; */
    while (x < 100) {
        after_first_iteration = loop_was_entered;
        loop_was_entered = 1;
        x += 1;
        /*@ assert after_first_iteration ==> security_status(y) == private; */
        // the equivalent of if (secret) continue;
        switch (secret) {
        case 0u:
            break;
        default:
            continue;
        }
        y = 1;
    }
    /*@ assert loop_was_entered ==> security_status(x) == private; */
    /*@ assert !loop_was_entered ==> security_status(x) == public; */
    /*@ assert loop_was_entered ==> security_status(y) == private; */
    /*@ assert !loop_was_entered ==> security_status(y) == public; */

    return 0;
}
