/* run.config_qualif
   OPT: -wp -wp-model Typed -wp-par 1 -wp-prop="-qed_ko,-ko,-A"
   OPT: -wp -wp-model Typed -wp-par 1 -wp-prop="qed_ko,ko,-A" -wp-steps 50
   OPT: -wp -wp-model Typed -wp-par 1 -wp-prop="A" -wp-msg-key print-generated
*/

/*@ axiomatic S {
  logic unsigned char t;
  }

  // here, the typing axiom should not be generated
  logic unsigned char x(unsigned char* p) = *p ;
 */

/*@ ensures qed_ok: 0<=t<256 ;
  @ ensures qed_ko: 0<=t<128 ; */
void f(void) {return;}

//@ ensures A: *p == x(p) <= 255 ;
void g(unsigned char* p){}
