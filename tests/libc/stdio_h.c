#include <stdio.h>
#include <stdlib.h>
#include "__fc_builtin.h"
volatile int nondet;
int caller_stub_for_vasprintf(char **strp, const char *fmt, ...);
int caller_stub_for_vscanf(const char * restrict format, ...);

int main() {
  FILE *f = fopen("/dev/urandom", "r");
  if (!f) return 1;
  int r = fseek(f, 0L, SEEK_SET);
  if (nondet) {
    fseek(NULL, 0L, SEEK_CUR); // must fail
    //@ assert \false;
  }
  if (nondet) {
    // to obtain an invalid value for whence, any interval containing at
    // least 4 elements must contain an invalid value
    int invalid_whence = Frama_C_interval(0, 3);
    if (invalid_whence != SEEK_SET && invalid_whence != SEEK_CUR &&
        invalid_whence != SEEK_END) {
      fseek(f, 42, invalid_whence); // must fail
      //@ assert \false;
    }
  }
  FILE *tmp = tmpfile();
  if (!tmp) return 2;
  fseek(tmp, 0L, SEEK_SET);
  fseeko(tmp, 0, SEEK_SET);
  long told = ftell(tmp);
  off_t toldo = ftello(tmp);
  fclose(tmp);

  FILE *redirected = freopen("/tmp/mytmp.txt", "w+", stdout);
  if (!redirected) return 3;
  printf("redirected to file");
  fclose(redirected);

  char fgets_buf0[1];
  char *fgets_res = fgets(fgets_buf0, 1, f); // ok
  if (!fgets_res) return 1;
  //@ check \initialized(&fgets_buf0[0]);
  if (nondet) {
    fgets(fgets_buf0, 2, f); // error: buf too small
    //@ assert unreachable: \false;
  }

  fpos_t pos;
  fgetpos(f, &pos);
  fsetpos(f, &pos);
  int res_fclose = fclose(f);

  if (nondet) {
    // Test asprintf without a C stub; the specification
    // uses an unsupported 'allocates' clause, so 's' will
    // point to invalid memory.
    char *s;
    r = asprintf(&s, "bla %s", 42);
    if (r == -1) return 1;
    printf("%s", s);
    free(s);
  }

  if (nondet) {
    // Test vasprintf without a C stub; the specification
    // uses an unsupported 'allocates' clause, so 's' will
    // point to invalid memory.
    char *s;
    r = caller_stub_for_vasprintf(&s, "bla %s", 42);
    if (r == -1) return 1;
    printf("%s", s);
    free(s);
  }

  fmemopen(0, 1, "w+"); // test to check that Eva emits warning about stdio.c

  char vscanf_s[10];
  char vscanf_d;
  r = caller_stub_for_vscanf("%d %s", &vscanf_d, vscanf_s);
  if (r) {
    //@ assert \initialized(&vscanf_d);
    Frama_C_show_each_must_be_reachable(vscanf_d);
  }
  return 0;
}

int caller_stub_for_vasprintf(char **strp, const char *fmt, ...) {
  // Fun fact: the code is the same as asprintf's stub in stdio.c
  va_list args;
  va_start(args, fmt);
  int res = vasprintf(strp, fmt, args);
  va_end(args);
  return res;
}

int caller_stub_for_vscanf(const char * restrict format, ...) {
  va_list args;
  va_start(args, format);
  int res = vscanf(format, args);
  va_end(args);
  return res;
}
