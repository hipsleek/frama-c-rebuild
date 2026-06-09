/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_GETOPT_H
#define __FC_GETOPT_H
#include "features.h"
__PUSH_FC_STDLIB
#include <unistd.h>
__BEGIN_DECLS


/* GNU specific */
struct option
{
  const char *name;
  int has_arg;
  int *flag;
  int val;
};

# define no_argument		0
# define required_argument	1
# define optional_argument	2


/*@
  assigns \result, optopt \from
    argv[0 .. argc-1][0..],
    indirect:argc,
    indirect:shortopts[0..], indirect:longopts[0..];
  assigns *(longopts[0..].flag) \from
    longopts[0..].val,
    indirect:argc, indirect:argv[0 .. argc-1][0..],
    indirect:shortopts[0..], indirect:longopts[0..];
  assigns optind \from
    optind,
    indirect:argc, indirect:argv[0 .. argc-1][0..],
    indirect:shortopts[0..], indirect:longopts[0..];
  assigns optarg \from
    argv[0 .. argc-1],
    indirect:argc,
    indirect:shortopts[0..], indirect:longopts[0..];
 */
extern int getopt_long (int argc, char *const argv[],
			const char *shortopts,
			const struct option *longopts, int *longind);

/*@
  assigns \result, optopt \from
    argv[0 .. argc-1][0..],
    indirect:argc,
    indirect:shortopts[0..], indirect:longopts[0..];
  assigns *(longopts[0..].flag) \from
    longopts[0..].val,
    indirect:argc, indirect:argv[0 .. argc-1][0..],
    indirect:shortopts[0..], indirect:longopts[0..];
  assigns optind \from
    optind,
    indirect:argc, indirect:argv[0 .. argc-1][0..],
    indirect:shortopts[0..], indirect:longopts[0..];
  assigns optarg \from
    argv[0 .. argc-1],
    indirect:argc,
    indirect:shortopts[0..], indirect:longopts[0..];
 */
extern int getopt_long_only (int argc, char *const argv[],
			     const char *shortopts,
			     const struct option *longopts, int *longind);

__END_DECLS

__POP_FC_STDLIB
#endif
