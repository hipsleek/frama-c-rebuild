/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DLFCN_H
#define __FC_DLFCN_H
#include "features.h"
__PUSH_FC_STDLIB
#define RTLD_LAZY 1
#define RTLD_NOW 2
#define RTLD_GLOBAL 3
#define RTLD_LOCAL 4
__BEGIN_DECLS

/*@
  assigns \result \from indirect:file[0..], indirect:mode; //missing: from 'filesystem';
*/
extern void *dlopen(const char *file, int mode);

/*@
  assigns \result \from handle, indirect:name[0..]; //missing: from 'filesystem';
*/
extern void *dlsym(void *handle, const char *name);

/*@
  assigns \result \from indirect:handle; //missing: from 'filesystem';
*/
extern int dlclose(void *handle);

__FC_EXTERN char __fc_dlerror[64];

/*@
  assigns \result \from &__fc_dlerror; //missing: from 'filesystem';
*/
extern char *dlerror(void);

__END_DECLS
__POP_FC_STDLIB
#endif

