/*  run.config*
    COMMENT: Test implicit flows due to loops with explicit break.
    STDOPT: +"-eva-slevel 5"
*/

extern unsigned int user_input;
extern unsigned int __fc_private secret;

int main(void) {
    int x, loop_was_entered;

    x = user_input;
    /*@ assert security_status(x) == public; */
    loop_was_entered = 0;
    while (x < 100) {
        loop_was_entered = 1;
        x++;
        if (x >= secret) break;
    }
    /*@ assert loop_was_entered ==> security_status(x) == private; */
    /*@ assert !loop_was_entered ==> security_status(x) == public; */

    return 0;
}
