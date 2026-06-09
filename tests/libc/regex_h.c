// Example based on REGCOMP(3P)'s man page
#include <regex.h>

int match(const char *string, char *pattern) {
  int status;
  regex_t re;

  if (regcomp(&re, pattern, REG_EXTENDED|REG_NOSUB) != 0) {
    return (0);
  }
  status = regexec(&re, string, (size_t) 0, 0, 0);
  regfree(&re);
  if (status != 0) {
    return (0);
  }
  return (1);
}

int main() {
  char *str = "a haystack";
  char *pattern = "hay";
  int r = match(str, pattern);
  return r;
}
