/*  run.config*
    COMMENT: Test for loops.
*/

extern unsigned int user_input;
extern unsigned int __fc_private secret;

int main(void) {
    unsigned int i, sum = 0;
    /*@ assert security_status(i) == public; */
    /*@ assert security_status(sum) == public; */
    for (i = 0; i <= secret; i++) {
        sum += i;
    }
    /*@ assert security_status(i) == private; */
    /*@ assert security_status(sum) == private; */

    return 0;
}
