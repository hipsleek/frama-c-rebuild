/*  run.config*
    COMMENT: Test side-effect analysis of early return out of loop.
*/

extern int __fc_private secret;

int compute(int n) {
    do {
        if (secret > n) {
            return n + 1;
        }
        n--;
    } while (n > 0);
    return n;
}

int main(void) {
    int result;
    /*@ assert security_status(result) == public; */
    result = compute(100);
    /*@ assert security_status(result) == private; */
    return result;
}
