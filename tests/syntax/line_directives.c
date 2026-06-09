/* run.config*
   FILTER: sed "s:$PWD/:PWD/:g"
   STDOPT: -no-print
 */
# 1 "non-existing-file.c"
int f(void) {} // to get a warning with a line number

# 2 "line_directives.c"
int g(void) {} // to get a warning with a line number

# line 3 "line_directives.c"
int h(void) {} // to get a warning with a line number

# line 4
int i(void) {} // to get a warning with a line number

# 5
int j(void) {} // to get a warning with a line number

# 6 "line_directives.c" 3
int k(void) {} // to get a warning with a line number
