/*  run.config*
    COMMENT: Test side-effect analysis of continue statements.
    STDOPT: +"-eva-slevel 1"
*/

extern unsigned int __fc_private secret;
extern unsigned int one, two, three;

unsigned int compute(unsigned int n) {
    do {
        if (secret > n)
            continue;
        n--;
    } while (n-- > 0);
    return n;
}

unsigned int compute2(unsigned int n) {
    unsigned int i;
    if (0 < n) { // loop must be entered at least once
        for (i = 0; i < n; i++) {
            if (secret > n)
                continue;
            n--;
        }
        /*@ assert security_status(n) == private; */
        /*@ assert security_status(i) == private; */
    } else {
        i = secret;
    }
    return i;
}

int main(void) {
    unsigned int result;
    result = compute(one);
    /*@ assert security_status(result) == private; */
    result = compute2(one);
    /*@ assert security_status(result) == private; */

    return 0;
}
