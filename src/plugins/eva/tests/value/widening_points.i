/* run.config*
  STDOPT: +"-eva-msg-key widening"
*/

/* Loops built with goto should be seen as normal loops in the wto and thus
   independent widenings should be performed at start and at the loop head
   of the while */
void main(void)
{ 
    int i;
start:
    i = 5;
    //@ loop unroll 5;
    while (i--) {
      Frama_C_show_each(i);
    }
    goto start;
}
