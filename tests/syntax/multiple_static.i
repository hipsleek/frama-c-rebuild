/* run.config
OPT: %{dep:multiple_static_2.i} -print -keep-unused-functions all
*/

inline static int hash(int a, int b) {
  return 0;
}
