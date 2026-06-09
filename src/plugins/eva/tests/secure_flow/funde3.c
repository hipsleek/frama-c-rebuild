/*  run.config*
*/

int a;
int *b;

int *f(int r, int s, int *u);
int g(int u, int v);

void main(){
    b = &a;
    f(a, 1,b);
    g(a,*b);
}

int* f(int s, int s_status, int *t){
    int *t_status = 0;
    a+=1 ;
    /*@ assert security_status(a) == public; */
    return t_status;
}

int g(int u, int v){
    int u_status = 0;
    return u_status;
}
