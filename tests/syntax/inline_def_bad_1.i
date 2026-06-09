/* run.config
STDOPT: +"%{dep:./inline_def_bad_2.i}"
*/

extern inline int f() { return 1; }

int g() { return f(); }
