/*  run.config*
    COMMENT: Test basic security flow annotations (public, private) on
    COMMENT: globals, locals, arrays, pointers
*/

extern unsigned int *global_p;
extern unsigned int **global_pp;

/* We can annotate entire arrays, but not individual array elements. */
extern unsigned int __fc_public global_arr[20];
extern unsigned int __fc_private arr_2d[3][4];
extern unsigned int __fc_private *global_p_arr[10];

extern unsigned int __fc_private x, y;  /* note: annotation affects *both* vars */
// The following variant is a syntax error:
// int u, __fc_private v;

/* The following causes a warning. It is not meaningful to declare the
 * security status for a pointer's target; that status is inherited from
 * whatever the pointer points to. */
extern unsigned int __fc_private *annotated_p;

int main(void) {
    unsigned int __fc_public a;
    unsigned int __fc_private b;
    unsigned int /* security level not specified explicitly */ c;
    unsigned int *local_p = &c;

    a = x + 1u;
    b = y - 1u;
    c = a * b;

    /*@ assert security_status(c) == private; */
    global_p = &c;
    /*@ assert security_status(*global_p) == private; */

    /* more complex annotations */
    /*@ assert security_status(*local_p) == security_status(arr_2d[2][3]); */
    /*@ assert security_status(global_arr[0]) < security_status(a); */

    /* test array-to-pointer casts */
    global_p = global_arr;
    /*@ assert security_status(global_p[19]) == public; */
    global_pp = global_p_arr;

    return 0;
}
