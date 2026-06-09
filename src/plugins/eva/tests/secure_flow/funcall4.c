/*  run.config*
    COMMENT: Test information flow via function pc labels with transitive calls.
*/

extern int __fc_private secret;
int x, y;

// Set x unconditionally and y conditionally depending on the secret. After
// every call, the status of y must be secret; the status of x depends on
// the calling context.
void set_y(void) {
    y = 1;
}

void set_x(void) {
    x = 1;
    if (secret) {
        set_y();
    }
}

int main(void) {
    /*@ assert security_status(x) == public; */
    /*@ assert security_status(y) == public; */
    set_x();
    /*@ assert security_status(x) == public; */
    /*@ assert security_status(y) == private; */
    y = 0;
    /*@ assert security_status(y) == public; */
    if (secret) {
        set_x();
        /*@ assert security_status(x) == private; */
    }
    /*@ assert security_status(x) == private; */
    /*@ assert security_status(y) == private; */
    set_x();
    /*@ assert security_status(x) == public; */
}
