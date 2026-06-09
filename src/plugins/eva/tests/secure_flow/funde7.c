/*  run.config*
*/

/* TODO: Make sure the program transformation also transforms this contract,
 * adding something like
 * \valid_read(a_status_d2_summary) && \valid(*a_status_d2_summary); */

/*@ requires \valid_read(a) && \valid(*a); */
int main(int **a, int b);
int main(int **a, int b){
    **a = 0;
    b = 1;
    int l = b ;
    /*@ assert security_status(l) == public; */
    return l;
}
