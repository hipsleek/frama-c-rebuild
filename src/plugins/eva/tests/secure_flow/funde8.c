/*  run.config*
*/

int a;

int *b = &a;

void main(int x){
    a = 20;
}

int unreachable_function(int **a, int b){
    **a = 0;
    b = 1;
    int l = b ;
    // This assertion does not affect the monitoring pre-analysis because
    // this function is never called. We therefore do not need to monitor
    // the statuses of l and b.
    /*@ assert security_status(l) == public; */
    return l;
}
