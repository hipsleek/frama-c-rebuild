/* run.config
   STDOPT: +"-sparecode-analysis"
   STDOPT: +"-sparecode-analysis -main main_bis"
   STDOPT: +"-sparecode-analysis -main main_ter"

*/


int i0, o0;

/*@ assigns i0, o0 \from \nothing; */
void loop_body (void) ;

/*@ assigns *p_res \from indirect:p_res; */
void init (int * p_res) ;

int is_ko = -1;
void main () {
  init (&is_ko);
  if (is_ko)
    while (1)
      loop_body () ;
}

void main_bis (void) {
  init (&is_ko);
  if (is_ko)
    while (1) {
      loop_body () ;
      /*@ slice_preserve_expr o0 ;*/
      }
}

void main_ter () {
  init (&is_ko);
  if (is_ko)
    while (1) {
      /*@ slice_preserve_stmt ;*/
      loop_body () ;
      }
}
