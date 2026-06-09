/*  run.config*
    COMMENT: Test information flow via function pc labels.
*/

extern int __fc_private secret;
int x, y;

// Set x to a constant. Although constants are public, this might still make
// x private if called in a private context.
void set_x(void) {
    x = 1;
}

int main(void) {
    /*@ assert security_status(x) == public; */
    set_x();
    /*@ assert security_status(x) == public; */
    if (secret) {
        set_x();
        /*@ assert security_status(x) == private; */
    }
    /*@ assert security_status(x) == private; */
    set_x();
    /*@ assert security_status(x) == public; */
}
