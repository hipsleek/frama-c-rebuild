/*  run.config*
    COMMENT: Test basic properties of structure field access.
*/

struct simple {
    int s_x;
    float s_f;
};

struct array {
    int a_arr[3];
};

extern int __fc_private secret;

int main(void) {
    struct simple s;
    s.s_x = 42;
    s.s_f = 42.0f;

    struct array a;
    a.a_arr[0] = 0;
    a.a_arr[1] = 1;
    /*@ assert security_status(a.a_arr) == public; */
    a.a_arr[2] = secret;
    /*@ assert security_status(a.a_arr) == private; */

    return 0;
}
