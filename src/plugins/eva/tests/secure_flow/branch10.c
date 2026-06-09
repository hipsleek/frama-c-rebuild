/*  run.config*
    COMMENT: Test logical-or operator.
*/

extern int __fc_private secret;
extern int __fc_public not_secret;

int main(void) {
    int x = 0;
    if (secret < 0 || secret > 10) {
        x = 1;
    }
    /*@ assert security_status(x) == private; */

    // same thing, with deeper nesting
    int y = 0;
    if (secret < 0 || secret == 5 || secret > 10) {
        y = 1;
    }
    /*@ assert security_status(y) == private; */

    // interaction between public and secret disjuncts
    int z = 0;
    if (secret < 0 || not_secret > 10) {
        z = 1;
    }
    /*@ assert security_status(z) == private; */
    z = 0;
    if (not_secret < 0 || secret > 10) {
        z = 1;
    }
    // the secret condition might not be evaluated here, so we don't know
    // anything about the final status
    /*@ assert security_status(z) == public ||
               security_status(z) == private; */

    return 0;
}
