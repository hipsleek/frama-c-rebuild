/* run.config
 DEPS: compile_commands.json
 COMMENT: parsing option are defined in the default json file "compile_commands.json"
   OPT: -compilation-db ./ -print
 DEPS:
   OPT: %{dep:./jcdb2.c} -compilation-db %{dep:./with_arguments.json} -print
 MODULE: @PTEST_NAME@
   OPT: -compilation-db %{dep:./with_arguments.json}
 MODULE:
 DEPS: file_without_main.c
   EXECNOW: LOG list_files.res LOG list_files.err %{bin:frama-c-script} list-files %{dep:./compile_commands_working.json} > ./list_files.res 2> ./list_files.err
*/

#include <stdio.h>

#ifdef TOUNDEF
#error TOUNDEF must be undefined by the compilation database
#endif

int main () {
  char *s = DOUBLE_SINGLE("a ");
  #ifndef __FRAMAC__
  printf("%s\n", s); // for GCC debugging
  #endif
  return MACRO_FOR_INCR(TEST) - TEST2; }
