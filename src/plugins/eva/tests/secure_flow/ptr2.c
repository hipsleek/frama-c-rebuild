/*  run.config*
*/

int x[10];

int *y = &x[2];

int n =10;

void main(){

    *(5+y) = n;
    /*@ assert security_status(n) == public; */
}
