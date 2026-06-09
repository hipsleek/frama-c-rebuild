/*  run.config*
    COMMENT: Test function side-effect analysis with arrays.
*/

int global_arr[3];
int write_count;

/*@ requires \valid(p+index);
    assigns write_count, p[index];
 */
void write(int *p, int index, int value) {
    write_count++;
    p[index] = value;
}

extern int __fc_private secret;

int main(void) {
    int secret_idx = !!secret;

    write(global_arr, 0, 1);
    /*@ assert security_status(global_arr) == public; */
    write(global_arr, secret_idx, 1);
    /*@ assert security_status(global_arr) == private; */

    int conditional_arr[3] = { 0, 0, 0 };
    /* @ assert security_status(conditional_arr) == public; */
    if (0 <= secret && secret < 3) {
        write(conditional_arr, 0, 1);
    }
    /*@ assert security_status(conditional_arr) == private; */

    int arr[3] = { 0, 0, 0 };
    /* Ideally, the following assertion should hold. However, the side
     * effect analysis is not yet context-sensitive, so the above
     * conditional call to write() causes a private update of arr. This is
     * because arr and conditional_arr are treated as aliasing via write()'s
     * first argument. */
    /* @ assert security_status(arr) == public; */
    write(arr, 2, secret);
    /*@ assert security_status(arr) == private; */
}
