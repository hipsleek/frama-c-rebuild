void f ( int k ) {
  struct { int x,y,z[4]; } s ;
  s.x = 1 ;
  s.z [ k ] = 0;
}
