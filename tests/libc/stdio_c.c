#include <stdlib.h>
#include <string.h>
#include "stdio.c"
#include "__fc_builtin.h"

volatile int nondet;



int main() {
  FILE *stream;
  char *line = NULL;
  size_t len = 0;
  size_t total_len = 0;
  ssize_t read;
  stream = fopen("/etc/motd", "r");
  if (!stream) return 1;
  while ((read = getline(&line, &len, stream)) != -1) {
    //@ assert read_ok: line != \null;
    total_len += strlen(line);
    //@ assert read_bytes: strlen(line) == read;
    //@ assert allocated_enough: len >= strlen(line);
  }
  free(line);
  fclose(stream);

  if (nondet) {
    // asprintf's stub calls vasprintf's stub, so this test
    // also works for vasprintf.
    char *s;
    int r = asprintf(&s, "bla %s", 42);
    if (r == -1) return 1;
    printf("%s", s);
    free(s);
  }

  if (nondet) {
    // Test fmemopen, and that the resulting stream can be used.
    char mode[3] = {0};
    if (nondet) mode[0] = 'w';
    if (nondet) mode[1] = '+';
    stream = fmemopen(NULL, 3 * sizeof(int), mode);
    if (!stream) return 1;
    int arr[3] = {42, 10, 1};
    int r = fwrite(arr, sizeof(int), 3, stream);
    //@ assert r <= 3;
  }

  int c = getchar();
  Frama_C_show_each_getchar(c);
  c = fgetc(stdin);
  Frama_C_show_each_fgetc(c);
  char buf[10];
  char *r = fgets(buf, 10, stdin);
  if (r) {
    //@ assert at_least_one_char: \initialized(&buf[0]);
  }

  return 0;
}
