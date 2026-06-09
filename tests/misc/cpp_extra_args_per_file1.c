/* run.config
   MODULE: cpp_extra_args_per_file
   OPT: -no-autoload-plugins -cpp-extra-args="-DGLOBAL" -cpp-extra-args-per-file ./@PTEST_NAME@.c:'-DFILE1 -DMACRO_WITH_QUOTES="\"hello world"\"',./cpp_extra_args_per_file2.c:"-DFILE2" -print -then %{dep:./cpp_extra_args_per_file2.c} %{dep:./cpp_extra_args_per_file3.c}
 */

#ifndef GLOBAL
#error GLOBAL must be defined
#endif

#ifndef FILE1
#error FILE1 must be defined
#endif

#ifdef FILE2
#error FILE2 must NOT be defined
#endif

// defined in cpp_extra_args_per_file3.c
extern int f(void);

int main() {
  char *a = MACRO_WITH_QUOTES;
  return f();
}
