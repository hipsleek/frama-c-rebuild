/* run.config*

  STDOPT: #"-main f"
*/
int f(int x) {
/* Here we are */
/*@ loop unfold 10; */
  while(1) { return 0 ;}
  return 2;
}
