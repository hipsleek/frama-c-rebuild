/*  run.config*
    COMMENT: Test function pointers that can be resolved unambiguously.
*/

extern int __fc_private secret;

int x, y;

void write_x(int x_arg) {
    x = x_arg;
}

void write_y(int y_arg) {
    y = y_arg;
}

struct func_descriptor {
    void (*f)(int);
};

struct func_descriptor funcs[] = {
    { write_x }, { write_y }
};

int main(void) {
    funcs[0].f(secret);  // write x
    funcs[1].f(42);      // write y

    /*@ assert security_status(x) == private; */
    /*@ assert security_status(y) == public; */

    x = y = 0;

    void (*p)(int) = write_x;
    void (*q)(int) = write_y;

    p(secret);  // write x
    q(42);      // write y

    /*@ assert security_status(x) == private; */
    /*@ assert security_status(y) == public; */

    return 0;
}
