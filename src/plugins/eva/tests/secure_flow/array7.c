/*  run.config*
*/

int a = 0;
int b = 1;

int *pa = &a;
int *pb = &b;

int t[2];

int *x[2]; // an array of two pointers to int

int (*y)[2]; // a pointer to an array of two int

int *(*z)[2]; // a pointer to an array of two pointers to int

/*
    Go right when you can, go left when you must

*/

void main(){
    t[0] = 3;
    t[1] = 4;

    x[0] = pa;
    x[1] = pb;

    y =  &t;

    z = &x;

    *(*z)[1] = 0;
    /*@ assert security_status(*z) == public; */

    t[1] = *(*z)[1] ;
}
