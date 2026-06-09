/* run.config
 OPT: -print -kernel-warn-key=annot-error=warning
PLUGIN: eva,inout,scope
 OPT: -eva @EVA_CONFIG@ -main g -print -no-annot
 */
//@ assigns *x;
void f(void *x);
void g() {
  int y;
  int* x = &y;
  f(x);
}
