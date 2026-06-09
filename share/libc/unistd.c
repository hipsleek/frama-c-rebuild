/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "unistd.h"
#include "__fc_builtin.h"
#include "string.h"
#include "getopt.h"
__PUSH_FC_STDLIB

volatile char __fc_hostname[HOST_NAME_MAX];
volatile char __fc_ttyname[TTY_NAME_MAX];
char __fc_crypt[256];
char __fc_getlogin[LOGIN_NAME_MAX];
char __fc_getpass[16];

int optind = 1;
char *optarg;
int opterr = 1; // initial value is not zero (zero silences error messages)

/* Note this implementation only supports the POSIXLY_CORRECT behaviour where
   the processing of arguments stops whenever a nonoption argument is
   encountered. A more general implementation would permute the argv contents
   so that eventually all the nonoptions are at the end. */

int getopt(int argc, char * const argv[], const char *optstring)
{
  if (optind >= argc || Frama_C_nondet(0, 1)) {
    // No more options
    return -1;
  }
  else {
    // Option found at index ind
    int ind = Frama_C_interval(optind, argc - 1);

    // Move optind forward
    optind = Frama_C_interval(ind, argc);

    // Retrieve the argument length
    int len = strlen(argv[ind]);
    //@ admit len > 0; // The argument is necessarily nonempty

    // Choose an option character
    char c = argv[ind][Frama_C_interval(0, len - 1)];

    if (Frama_C_nondet(0, 1)) { // Normal case
      // Set optarg
      if (Frama_C_nondet(0, 1)) {
        // If there is an argument to the option, point to it
        optarg = &argv[ind][Frama_C_interval(0, len - 1)];
      }
      else {
        optarg = 0;
      }
      return c;
    } else { // Error case
      optopt = c;
      return optstring[0] == ':' ? ':' : '?';
    }
  }
}

int getopt_long (int argc, char *const argv[],
                 const char *optstring,
                 const struct option *longopts, int *longind)
{
  // (Possibly) same behaviour as getopt() if no/short option is found
  if (Frama_C_nondet(0, 1)) {
    return getopt(argc, argv, optstring);
  } else {
    // found long option at index ind
    int ind = Frama_C_interval(0, INT_MAX);
    //@ admit \valid(&longopts[ind]) && longopts[ind].name != 0;
    const struct option *p = &longopts[ind];

    // Move optind forward
    optind = Frama_C_interval(ind, argc);

    // Retrieve the argument length
    int len = strlen(argv[ind]);
    //@ admit len > 0; // The argument is necessarily nonempty

    // Set longind
    if (longind) {
      *longind = ind;
    }

    // Set optarg
    if (Frama_C_nondet(0, 1)) {
      // If there is an argument to the option, point to it
      optarg = &argv[ind][Frama_C_interval(0, len - 1)];
    }
    else {
      optarg = 0;
    }

    // Set or return the value
    if (p->flag) {
      *(p->flag) = p->val;
      return 0;
    }
    else {
      return p->val;
    }
  }
}

int getopt_long_only(int argc, char *const argv[],
                     const char *optstring,
                     const struct option *longopts, int *longind) {
  return getopt_long(argc, argv, optstring, longopts, longind);
}

__POP_FC_STDLIB
