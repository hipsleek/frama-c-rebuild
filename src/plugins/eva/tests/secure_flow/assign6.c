/*  run.config*
    COMMENT: Test direct struct assignments. The context flows to all fields.
*/

struct pair {
    int a;
    double b;
};

struct nested {
    int n;
    struct pair p;
};

extern int __fc_private secret;

int main(void) {
    struct nested s = { 0, { 1, 2.0 } };
    struct nested t = { 3, { 4, 5.0 } };
    struct nested u;

    /*@ assert security_status(s.n) == public; */
    /*@ assert security_status(s.p.a) == public; */
    /*@ assert security_status(s.p.b) == public; */
    /*@ assert security_status(t.n) == public; */
    /*@ assert security_status(t.p.a) == public; */
    /*@ assert security_status(t.p.b) == public; */
    if (secret) {
        u = s;
    } else {
        u = t;
    }
    /*@ assert security_status(u.n) == private; */
    /*@ assert security_status(u.p.a) == private; */
    /*@ assert security_status(u.p.b) == private; */

    return 0;
}
