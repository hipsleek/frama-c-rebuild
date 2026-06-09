/* run.config
EXECNOW: BIN foo".c cp %{dep:./foo.src} foo\".c
DEPS: ./foo".c
STDOPT: +"./foo\\\".c"
*/

extern int test = 1;
