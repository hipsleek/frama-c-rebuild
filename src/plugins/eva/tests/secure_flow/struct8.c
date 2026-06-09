/*  run.config*
    COMMENT: Test unions of structs.
*/

struct ab {
    int a[10];
    int b;
};

struct cd {
    int c;
    int b[5];
};

union foo {
    struct ab ab;
    struct cd cd;
};

extern int __fc_private secret;

int main(void) {
    union foo u = { { 0 } };

    u.cd.c = secret;
    /*@ assert security_status(u.cd.c) == private; */

    return 0;
}
