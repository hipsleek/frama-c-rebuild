/* run.config
STDOPT: -acsl-import %{dep:./cfg.acsl} -print -acsl-import-debug 2 -acsl-import-msg-key "*"
 */
void case_label (unsigned i) {
  i++;
  switch (i) {
  default:
  D0: d0: {
      i--;
    D1: d1: case 1: C1: c1: i++;
    D2: d2: case 2: C2: c2: i--;
    }
  }
}

void label (unsigned i) {
  i++ ;
 L0: //@ assert i == \at(i,Pre) + 1 ;
     //@ ensures i == \old(i) + 1 ;
 L1: i--;
  i-- ;
  i++ ;
 L2: //@ assert i == \at(i,Pre) ;
     //@ ensures i == \old(i) - 1 ;
 L3: i--;
  i++ ;
}
void label_start_end (unsigned i) {
 L0: //@ assert i == \at(i,Pre) ;
     //@ ensures i == \old(i) + 1 ;
 L1: i--;
  i-- ;
  i++ ;
 L2: //@ assert i == \at(i,Pre) + 1 ;
     //@ ensures i == \old(i) - 1 ;
 L3: i--;
}
