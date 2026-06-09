/*  run.config*
    COMMENT: Test weak/strong updates for arrays and pointers.
*/

extern int __fc_private x;
extern int __fc_public y;
extern int __fc_private arr[10];

int main(void) {
    int *p = &x;
    int x_idx = !!x;

    *p = 23;  // causes a strong update of x
    /*@ assert security_status(x) == public; */
    /*@ assert security_status(*p) == public; */

    /*@ assert security_status(x_idx) == private; */

    p = arr;
    /*@ assert security_status(p) == public; */
    /*@ assert security_status(*p) == private; */
    p = &arr[1];
    /*@ assert security_status(p) == public; */
    /*@ assert security_status(*p) == private; */

    *p = 0;
    p[1] = 1;
    *(p + 2) = 2;
    p[x_idx] = x;
    p++;
    *p = 42;
    /*@ assert security_status(*p) == public; */

    p = &y;
    /*@ assert security_status(*p) == public; */
    *p = 42;  // strong update of y
    /*@ assert security_status(y) == public; */
    /*@ assert security_status(*p) == public; */

    return 0;
}
