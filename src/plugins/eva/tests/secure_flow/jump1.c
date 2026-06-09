/*  run.config*
    COMMENT: Test side-effect analysis of forward goto due to early return.
*/

extern int __fc_private secret;

int compute(int n) {
    if (secret) {
        return n + 1;
    }
    return n;
}

int main(void) {
    int result;
    /*@ assert security_status(result) == public; */
    result = compute(0);
    /*@ assert security_status(result) == private; */
    return result;
}
