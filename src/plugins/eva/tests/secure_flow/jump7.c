/*  run.config*
    COMMENT: Test analysis of generalized ||-like jump.
*/

extern int __fc_private secret;
extern int c1, c2;
int x1, x2, x3;

int main(void) {
    // This is similar to the pattern generated for an || operator, but the
    // jump from one branch goes into a deeply nested block in the other
    // branch. Make sure all side effects are properly captured. Based on an
    // example from Julien.
    if (secret) {
        goto L;
    } else {
        x1 = 1;
        if (c1) {
            x2 = 2;
            if (c2) {
                x3 = 3;
                L: ;
            }
        }
    }
    /*@ assert security_status(x1) == private; */
    /*@ assert security_status(x2) == private; */
    /*@ assert security_status(x3) == private; */
    return 0;
}
