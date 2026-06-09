/*  run.config*
*/

int a;
int b;
int c;
int d;

int *m = &a;
int *n = &b;
int *o = &c;
int *p = &d;

int *x[4] = {&a, &b, &c, &d};

int **y[4] = { &m, &n, &o, &p};

void main(){

    **y[1] = 0;

    /*@ assert security_status(**y[1]) == public; */
}
