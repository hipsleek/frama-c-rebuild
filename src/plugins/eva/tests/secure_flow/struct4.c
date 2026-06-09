/*  run.config*
    COMMENT: Test complex linked data structures of structs/arrays/pointers.
*/

struct Y {
    int z[4];
};

struct X {
    struct Y *y[3];
};

struct Y y;
struct X x_obj;
struct X *x = &x_obj;

struct P {
    int p_y;
};

int main(void) {
    x_obj.y[2] = &y;

    x->y[2]->z[3] = 42;

    struct P p_obj;
    p_obj.p_y = 0;
    int *p;
    p = &p_obj.p_y;

    struct Y *some_y_ptr = x->y[0];

    /*@ assert security_status(x->y[2]) == public; */

    return x->y[2]->z[3];
}
