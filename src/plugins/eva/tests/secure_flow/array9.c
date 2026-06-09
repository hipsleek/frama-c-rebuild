/*  run.config*
    COMMENT: Test tricky array properties
*/

extern unsigned char __fc_private secret;
int array[512];

extern int __fc_public x, y, z;
int *ptr_array[2] = { &x, &y };

int *q;

int main(void) {
    /*@ assert security_status(x) == public; */
    /*@ assert security_status(y) == public; */
    /*@ assert security_status(ptr_array) == public; */

    int secret_idx = !!secret;
    /*@ assert security_status(secret_idx) == private; */
    *ptr_array[secret_idx] = 2;
    /* This assignment has made both x and y private, but neither Value nor
     * WP (with Alt-Ergo) seem to be strong enough to prove this. */
    /*@ assert security_status(*ptr_array[secret_idx]) == private; */
    /*@ assert security_status(x) == private; */
    /*@ assert security_status(y) == private; */

    /* We can make both x and y public again. */
    x = 0;
    y = 0;
    /*@ assert security_status(x) == public; */
    /*@ assert security_status(y) == public; */
    /*@ assert security_status(ptr_array) == public; */
    z = *ptr_array[secret_idx];
    /*@ assert security_status(z) == private; */
    /*@ assert security_status(x) == public; */
    /*@ assert security_status(y) == public; */

    /*@ assert security_status(array) == public; */

    /* This may affect any array element 0..UCHAR_MAX. We set the array's
     * summary label to ensure that any element of the array is always read
     * as private. */
    array[secret] = 1;
    /*@ assert security_status(array) == private; */
    /*@ assert security_status(array[0]) == private; */
    array[array[secret]] = 1;
    /*@ assert security_status(array) == private; */
    /*@ assert security_status(array[0]) == private; */

    int *p = &array[0];
    *(p + array[secret]) = 1;
    /*@ assert security_status(*p) == private; */
}
