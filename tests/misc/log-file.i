/* run.config
 FILTER: sed 's|Your Frama-C version is.*|Your Frama-C version is VERSION|'
   LOG: log-file-kernel-warnings.txt
   LOG: log-file-kernel-results.txt
   LOG: log-file-feedback.txt
   LOG: log-file-value-all.txt
   LOG: log-file-value-default.txt
   LOG: plugin-log-all.txt
PLUGIN: @EVA_PLUGINS@
   STDOPT: #"-kernel-log w:./log-file-kernel-warnings.txt,r:./log-file-kernel-results.txt -eva-log f:./log-file-feedback.txt,afewr:./log-file-value-all.txt -eva-log :./log-file-value-default.txt -then -kernel-log f:./log-file-feedback.txt"
 MODULE: plugin_log
   OPT: -kernel-msg-key foo-category -kernel-log=a:./plugin-log-all.txt
   DONTRUN: test disabled due to non-deterministic errors in CI
 */
int f(void); // generates kernel warning (missing spec)

//@ assigns \result;
int g(void); // generates value warning (missing \from)

int main() {
  f();
  int r = g();
  for (int i = 0; i < 1; i++); // generates value feedback
  return 0;
}
