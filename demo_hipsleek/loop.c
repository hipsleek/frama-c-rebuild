/*[SL]
   requires i = 0
   ensures res = 10;
*/
int count_to_ten(int i) {
  /*[SL_loop]
     requires true
     ensures i < 10 & i' = 10 or i >= 10 & i' = i;
  */
  while (i < 10) {
    i = i + 1;
  }
  return i;
}
