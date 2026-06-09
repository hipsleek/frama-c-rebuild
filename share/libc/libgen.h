/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_LIBGEN_H
#define __FC_LIBGEN_H
#include "features.h"
#include "__fc_machdep.h"
#include "__fc_string_axiomatic.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

__FC_EXTERN char __fc_basename[__FC_PATH_MAX];

/*@ // missing: assigns path[0 ..], __fc_basename[0 ..] \from 'filesystem';
  requires null_or_valid_string_path: path == \null || valid_read_string(path);
  assigns path[0 ..], __fc_basename[0 ..] \from path[0 ..], __fc_basename[0 ..];
  assigns \result \from &__fc_basename, path;
  ensures result_points_to_internal_storage_or_path:
    \subset(\result, {&__fc_basename, path});
*/
extern char *basename(char *path);

__FC_EXTERN char __fc_dirname[__FC_PATH_MAX];

/*@ // missing: assigns path[0 ..], __fc_dirname[0 ..] \from 'filesystem';
  requires null_or_valid_string_path: path == \null || valid_read_string(path);
  assigns path[0 ..], __fc_dirname[0 ..] \from path[0 ..], __fc_dirname[0 ..];
  assigns \result \from &__fc_dirname, path;
  ensures result_points_to_internal_storage_or_path:
    \subset(\result, {&__fc_dirname, path});
*/
extern char *dirname(char *path);

__END_DECLS
__POP_FC_STDLIB
#endif
