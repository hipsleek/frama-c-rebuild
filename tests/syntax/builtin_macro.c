/* run.config
OPT: -machdep gcc_x86_64 -print
*/

#if defined(__GNUC__)
int x = 0;
#else
#error "__GNUC__ should be defined in gcc_x86_64 machdep"
#endif
