/*  run.config*
    COMMENT: Test use of variables via pointers before their declaration.
*/

void deref(int *p) {
    // This assignment must update the labels of a and b, but they are not
    // yet declared at this point, so the transformation has to shuffle
    // declarations around.
    *p = 42;
}

extern int __fc_private a;
extern int b;

int main(void) {
    deref(&a);
    /*@ assert security_status(a) == public; */
    deref(&b);
    return 0;
}
