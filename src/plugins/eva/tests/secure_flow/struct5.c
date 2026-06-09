/*  run.config*
    COMMENT: Test local initialization of structs.
*/

struct simple {
    int s_x;
    struct inner {
        int i_y;
    } s_inner;
};

struct array {
    struct inner a_inner[3];
    struct simple a_simple[3];
};

extern int __fc_private secret;

int main(void) {
    struct simple s = { .s_inner = { .i_y = secret } };

    struct array a = {
        {{0}, {1}, {secret}},
        { [2] = {3, {secret}}, [1] = {5, {6}}, [0] = {7, {8}}}
    };
    /*@ assert security_status(a.a_inner[0].i_y) == public; */
    /*@ assert security_status(a.a_simple[0].s_x) == public; */
    /*@ assert security_status(a.a_simple[0].s_inner.i_y) == public; */
    /*@ assert security_status(a.a_simple[2].s_inner.i_y) == private; */

    return 0;
}
