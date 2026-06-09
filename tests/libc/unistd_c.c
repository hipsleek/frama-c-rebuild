/*run.config
  STDOPT: +"%{dep:../../../share/libc/eva_stubs.c}"
  STDOPT: +"%{dep:../../../share/libc/eva_stubs.c}" +"-main eva_main_simple"
  STDOPT: +"%{dep:../../../share/libc/eva_stubs.c}" +"-main eva_main"
*/

#include "unistd.c"
#include "stdlib.h" // for atof

// Example data based on admesh (https://github.com/admesh/admesh)
int main_long(int argc, char **argv) {
  enum {rotate_x = 1000, rotate_y, rotate_z, merge, help, version,
        stretch, reverse_all, off_file, scale_xyz
  };

  struct option long_options[] = {
    {"exact",              no_argument,       NULL, 'e'},
    {"tolerance",          required_argument, NULL, 't'},
    {"iterations",         required_argument, NULL, 'i'},
    {"no-check",           no_argument,       NULL, 'c'},
    {"write-binary-stl",   required_argument, NULL, 'b'},
    {"write-off",          required_argument, NULL, off_file},
    {"stretch",            required_argument, NULL, stretch},
    {"merge",              required_argument, NULL, merge},
    {"version",            no_argument,       NULL, version},
    {NULL, 0, NULL, 0}
  };

  int c, x;
  double tolerance;
  char     *binf, *input_file;
  while((c = getopt_long(argc, argv, "et:i:cb:",
                         long_options, (int *) 0)) != -1) {
    switch(c) {
    case 0:
      break;
    case 'e':
      x = 1;
      break;
    case 't':
      tolerance = atof(optarg);
      break;
    case 'b':
      binf = optarg; /* comment from admesh: "I'm not sure if this is safe." */
      break;
    default:
      x = 3;
    }
  }

  if(optind == argc) {
    return 1;
  } else {
    input_file = argv[optind];
  }

  return 0;
}

int main(int argc, char **argv) {
  int r = getopt(argc, argv, "tes:");
  r = main_long(argc, argv);
  return 0;
}

int eva_main_simple() {
  int argc = 4;
  char *argv[] = {"program_name", "-this", "is a", "Test0"};
  return main(argc, argv);
}
