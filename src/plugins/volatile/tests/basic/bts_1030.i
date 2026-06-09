/* run.config
STDOPT:
 */
#line 1
//@ assigns \nothing;
extern int w(int volatile *p, int x);
//@ assigns \nothing;
extern int r(int volatile *p);

int volatile V;
//@ volatile V reads r writes w ;

// Pour que les fonction r et w ne disparaissent pas.
void main (void) {
  r(&V) ;
  w(&V, 1) ;
}

void f_check_crash(int x) {
  switch (x) {
  case 1 : {
      x = V + 1 ;
      break ; }

    default:
      break ;
    }
}

void f(int x) {
  switch (x) {
  case 1 :
      x = V + 1 ;
      break ;

    default:
      break ;
    }
}

void g(int x) {
  if (x) {
  CASE1 : {
      x = V + 1 ;
      goto BREAK ; }
  }
  else
    goto BREAK ;
  BREAK:;

}

//@ ensures  x == 1 || x == 2;
void wp_crash1 (int x) {
  switch (x) {
  case 1 :
      x = V + 1 ;
      break ;


    default:
      break ;
    }
}

//@ ensures  x == 1 || x == 2;
void wp_crash2 (int x) {
  switch (x) {
  case 1 :
      V = V + 1 ;
      break ;


    default:
      break ;
    }
}
