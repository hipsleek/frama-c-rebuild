/*  run.config*
*/

int x[10];

int *y[10];

int **z[10];

int ***u[10];

int ****w;

int a;

void main(){
    x[2] = 1;

    y[3] = x;

    z[7] = y;

    u[1] = z;

    w = u;

    a = *(*(*(*(w+1) + 7) + 3) + 2);
    /*@ assert security_status(z[7]) == public; */
}
