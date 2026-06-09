void foo(int* p __attribute__((wp_nullable)),
         int* q __attribute__((wp_nullable))){

}

// or equivalently:
//@ \wp::nullable_args p, q ;
void foo(int* p, int* q){

}
