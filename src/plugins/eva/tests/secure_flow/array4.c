/*  run.config*
*/

int a = 0;


int t[2];


int (*y)[2]; // a pointer to an array of two int


/*
    Go right when you can, go left when you must

*/

void main(){
    t[0] = 3;
    t[1] = 4;


    y =  &t;

    (*y)[1] = 3;

    a = t[1];
    /*@ assert security_status(a) == public; */
}
