/*  run.config*
    COMMENT: Test more general jumps from one branch into another.
*/

extern int __fc_private secret1;
extern int __fc_private secret2;
extern int __fc_private secret3;

// Based on an example by Julien.
int foo(void) {
    int x = 0, c = 0;
    if (secret1) {
        int a = 0;
        if (secret2) {
            int b = 1;
            a = 1;
            return b;
        } else {
            x = 2;
            goto L;
        }
    } else {
        c = 3;
        if (secret3 < c) {
            L: c++;
            return x;
        } else {
            return c;
        }
    }
    return x;
}

int main(void) {
    int x = foo();
    /*@ assert security_status(x) == private; */
    return 0;
}
