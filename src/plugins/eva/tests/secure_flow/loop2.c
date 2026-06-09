/*  run.config*
    COMMENT: Test flows due to loops in branches and branches in loops.
*/

extern unsigned int input;
extern unsigned int __fc_private secret;
unsigned int result;

// adapted from https://en.wikipedia.org/wiki/Euclidean_algorithm
void gcd(unsigned int a, unsigned int b) {
    while (a != b) {
        if (a > b) {
            a = a - b;
        } else {
            b = b - a;
        }
    }
    result = a;
}

// just some random deeply nested operations
void foo(unsigned int a, unsigned int b) {
    if (a % 2u == 0u) {
        while (b > 0u) {
            if (a > 0u) {
                while (b > a) {
                    b -= 1u;
                }
            }
        }
    } else {
        do {
            a -= 1u;
        } while (a > b);
    }
    result = a + b;
}

int main(void) {
    result = 0;
    /*@ assert security_status(result) == public; */
    gcd(input, secret);
    /*@ assert security_status(result) == private; */

    result = 0;
    /*@ assert security_status(result) == public; */
    gcd(secret, input);
    /*@ assert security_status(result) == private; */

    result = 0;
    /*@ assert security_status(result) == public; */
    foo(input, secret);
    /*@ assert security_status(result) == private; */

    result = 0;
    /*@ assert security_status(result) == public; */
    foo(secret, input);
    /*@ assert security_status(result) == private; */

    return result;
}
