/*  run.config*
    COMMENT: Test element-wise weak update of structs and unions.
*/

struct foo {
    int foo_a;
    float foo_b;
};

struct bar {
    int bar_a;
    float bar_b;
};

union baz {
    int baz_a;
    float baz_b;
};

struct foo foo;
struct bar __fc_private bar;
union baz baz;

// External functions with assigns annotations will force weak updates.
/*@ assigns *fooptr \from *barptr; */
void assign_struct(struct foo *fooptr, const struct bar *barptr);
/*@ assigns *bazptr \from *barptr; */
void assign_union(union baz *bazptr, const struct bar *barptr);

int main(void) {
    assign_struct(&foo, &bar);
    assign_union(&baz, &bar);

    /*@ assert security_status(foo.foo_a) == private; */
    /*@ assert security_status(baz.baz_b) == private; */

    return 0;
}
