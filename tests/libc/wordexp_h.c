/* Example based on:
   https://www.gnu.org/software/libc/manual/html_node/Wordexp-Example.html */

#include <stddef.h>
#include <sys/types.h>
#include <wordexp.h>

int main (const char *program, const char **options) {
  wordexp_t result;

  switch (wordexp (program, &result, 0)) {
  case 0:
    break;
  case WRDE_NOSPACE:
    wordfree (&result);
  default:
    return -1;
  }

  for (int i = 0; options[i] != NULL; i++) {
    if (wordexp (options[i], &result, WRDE_APPEND)) {
      wordfree (&result);
      return -1;
    }
  }

  wordfree (&result);
  return 0;
}
