int local_vla(int len){
  //@ ghost int a[len];
  //@ ghost a[4] = 32;
  return 42;
}

void formal_vla(int len) /*@ ghost (int a[len]) */{
  //@ ghost a[4] = 32;
}
