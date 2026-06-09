typedef unsigned char      uint8 ;

void null(void){
  uint8 buffer[sizeof(int*)] ;
  *((int**) buffer) = (void*) 0 ;

  int* r = *((int**) buffer) ;
  //@ check r == \null ;
}

int g ;

void addr_glob(void){
  uint8 buffer[sizeof(int*)] ;
  *((int**) buffer) = &g ;

  int* r = *((int**) buffer) ;
  //@ check r == &g ;
}

void addr_formal(int f){
  uint8 buffer[sizeof(int*)] ;
  *((int**) buffer) = &f ;
  int* r = *((int**) buffer) ;

  //@ check r == &f ;
}

void addr_local_ok(void){
  int l = 0;

  uint8 buffer[sizeof(int*)] ;
  *((int**) buffer) = &l ;

  int* r = *((int**) buffer) ;
  //@ check P: r == &l ;
}

void addr_local_ko(void){
  uint8 buffer[sizeof(int*)] ;

  {
    int l ;
    *((int**) buffer) = &l ;
  }

  int* r = *((int**) buffer) ;
  //@ check ! \valid(r) ;
}

//@ requires \valid(f);
void pointer_param(int *f){
  uint8 buffer[sizeof(int*)] ;
  *((int**) buffer) = f ;

  int* r = *((int**) buffer) ;
  //@ check r == f ;
}
