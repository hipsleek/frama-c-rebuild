/*  run.config*
    COMMENT: Test struct array members.
    STDOPT: +"-eva-slevel 1"
*/

struct abc {
    int a[10];
    int b;
};

extern int __fc_private secret;

int main(void) {
    int i;
    struct abc s = { { 0 } };

    for (i = 0; i < 10 && i < secret; i++) {
        s.a[i] = i;
    }

    /*@ assert security_status(s.a[0]) == private; */
    return 0;
}
