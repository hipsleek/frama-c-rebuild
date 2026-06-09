/* run.config
   EXIT: 1
   OPT: -kernel-warn-key parser:decimal-float=warning -pow-limit 40000000 */
int main() {
  double ok_max = 1e308;
  double too_big = 1e309;
  double ok_min_norm = 1e-308;
  double ok_min_denorm = 5e-324;
  double too_small = 1e-325;
  double dos1 = 1e-40000000;
  double dos2 = 1e-400000000;
  return 0;
}
