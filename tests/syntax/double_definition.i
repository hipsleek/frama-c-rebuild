/* run.config
EXIT: 1
OPT:
*/

void f() { }
// error (redefinition of f), but Frama-C shouldn't
// leave a backtrace for that...
void f() { }
