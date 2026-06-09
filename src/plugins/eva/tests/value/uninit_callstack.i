/* run.config*
 PLUGIN: @EVA_MAIN_PLUGINS@
   FILTER: sed -e 's/<....>/<????>/g'
   OPT: -eva @EVA_CONFIG@ -eva-msg-key=callstacks,callstack-hash,-progress -eva-no-results
*/
int *p, x;
void f(void)
{
  if (*p) x = 1;
}

int main(){
  int a;
  p = &a;
  f();
}
