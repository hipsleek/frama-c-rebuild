/* run.config
   PLUGIN: @EVA_PLUGINS@
   EXIT: 1
   STDOPT: +"-cpp-extra-args=\"-DKERNEL_ERROR\" -eva -print"
   STDOPT: +"-cpp-extra-args=\"-DEVA_ERROR\" -eva -print -eva-warn-key alarm=error"
*/

#ifdef KERNEL_ERROR
int main() {

  // Kernel deferred error, Eva is not run.

  int v;
  //@ ghost v = 1;
  //@ assert \false;
  return 0;
}
#endif

#ifdef EVA_ERROR
int main() {

  // Eva deferred error, print before treating the deferred error

  int * i;
  int a = *i;
  return 0;
}
#endif
