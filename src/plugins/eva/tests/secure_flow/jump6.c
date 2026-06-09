/*  run.config*
    COMMENT: Test side-effect analysis of nested loops with jumps.
    STDOPT: +"-eva-slevel 1"
*/

extern unsigned int __fc_private secret;
extern unsigned int one, two, three;

unsigned int compute(unsigned int n) {
    do {
        do {
            if (secret > n)
                return n;
            n--;
        } while (n > 0);
    } while (n-- > 0);
    // The expression n-- in the loop condition above introduces a variable
    // tmp which is local to the loop, i.e., not in scope at this point. The
    // transformation correctly removes it from the set of variables to be
    // updated here for the goto from the early return.
    return n;
}

unsigned int compute2(unsigned int n) {
    unsigned int i = n, j = 0, k = 10;
    while (j < k) {
        j += 2;
        /*@ assert security_status(j) == public; */
        if (n > 0) {
            do {
                i--;
                if (secret > n)
                    continue;
                n--;
            } while (i > 0);
            /*@ assert security_status(n) == private; */
        } else {
            n = secret;
        }
        k++;
        /*@ assert security_status(k) == public; */
    }
    return n;
}

int main(void) {
    unsigned int result;
    result = compute(one);
    /*@ assert security_status(result) == private; */
    result = compute2(one);
    /*@ assert security_status(result) == private; */

    return 0;
}
