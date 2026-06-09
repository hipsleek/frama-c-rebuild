/*  run.config*
    COMMENT: Test implicit flows due to switch statements.
*/

extern int __fc_private secret;

int a, b, c, d, e;

int main(void) {
    /*@ assert security_status(a) == public; */
    /*@ assert security_status(b) == public; */
    /*@ assert security_status(c) == public; */
    /*@ assert security_status(d) == public; */
    /*@ assert security_status(e) == public; */
    switch (secret) {
    case 0:
        a = 1;
        break;
    case 1:
        b = 2;
        break;
    case 2:
        c = 3;
        /* fall through */
    case 3:
        d = 4;
        break;
    default:
        e = 5;
    }
    /*@ assert security_status(a) == private; */
    /*@ assert security_status(b) == private; */
    /*@ assert security_status(c) == private; */
    /*@ assert security_status(d) == private; */
    /*@ assert security_status(e) == private; */

    return 0;
}
