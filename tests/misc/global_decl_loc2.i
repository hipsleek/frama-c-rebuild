/* run.config*
 COMMENT: note: the module global_decl_loc is also used by another test file (global_decl_loc.i)

 MODULE: global_decl_loc
   OPT: %{dep:./global_decl_loc.i}
*/
extern int g;

int main(void) {
  int a = g;
  return a;
}
