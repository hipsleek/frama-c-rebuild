/*  run.config*
    COMMENT: Test global initialization of structs.
*/

struct abc {
    int a; float b; double c;
};

struct pointers {
    int *p; float *q; double *r;
};

struct abc abc = { 42, 42.0f, 42.0 };

struct abc __fc_private secret_abc;
struct pointers __fc_private secret_pointers;

struct pointers pointers = { &secret_abc.a, &secret_abc.b, &secret_abc.c };

struct simple {
    int x;
    int *p;
};

struct simple __fc_private simple_arr_secret[2];
struct simple __fc_private simple_arr_initialized[2] = {
    { 0, &secret_abc.a }, { 1, &secret_abc.a }
};
struct simple __fc_public public_simple_arr[2];

struct pair {
    int p_a;
    float *p_b;
};

struct nested {
    struct pair pairs[2];
    struct pointers *p;
};

struct nested n = {
    { { 0, &abc.b }, { 1, &secret_abc.b } },
    &pointers
};

int main(void) {
    return 0;
}
