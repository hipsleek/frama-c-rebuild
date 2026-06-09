/* run.config
  OPT: -rte -then -region -then -print
  OPT: -warn-invalid-pointer -warn-unaligned-pointer -rte-initialized @all -rte -then -region -then -print
  OPT: -warn-invalid-pointer -warn-unaligned-pointer -rte-initialized @all -region -then -rte -then -print
*/

/*@ region *p, \allocated; */
int access(int *p) { return *p; }
