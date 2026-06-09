volatile int v;
int job(int x) {
  v=x;
  return v;
}

//@ logic integer wr_trans(integer status, int x) = status+x;
//@ logic integer rd_trans(integer status) = status-1;
//@ logic integer rd_value(integer status) = status;

int state_v = 0;

/*@ requires p == &v;
  @ assigns state_v;
  @ ensures state_v==wr_trans(\old(state_v),x);
  @*/
int wr_v (int volatile *p, int x) ;

/*@ requires p == &v;
  @ assigns state_v;
  @ ensures state_v==rd_trans(\old(state_v));
  @ ensures \result==rd_value(\old(state_v));
  @*/
int rd_v (int volatile *p) ;

//@ volatile v reads rd_v writes wr_v;

