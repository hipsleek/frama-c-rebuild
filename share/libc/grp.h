/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_GRP_H
#define __FC_GRP_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_uid_and_gid.h"
#include "__fc_define_size_t.h"
#include <errno.h>

__BEGIN_DECLS

struct group {
  char   *gr_name;
  gid_t   gr_gid;
  char  **gr_mem;
};

volatile struct group __fc_grp;

/*@
  //missing: assigns <all below> \from 'group database';
  assigns \result \from indirect:gid, &__fc_grp;
  assigns errno \from indirect:gid;
*/
extern struct group *getgrgid(gid_t gid);

/*@
  //missing: assigns <all below> \from 'group database';
  assigns \result \from indirect:name[0..], &__fc_grp;
  assigns errno \from indirect:name[0..];
*/
extern struct group *getgrnam(const char *name);

/*@
  //missing: assigns <all below> \from 'group database';
  assigns \result \from indirect:gid, indirect:bufsize;
  assigns *grp \from gid, indirect:bufsize;
  assigns buffer[0 .. bufsize-1] \from gid, indirect:bufsize;
  assigns *result \from grp;
  assigns errno \from indirect:gid, indirect:bufsize;
*/
extern int getgrgid_r(gid_t gid, struct group *grp, char *buffer,
                      size_t bufsize, struct group **result);

/*@
  //missing: assigns <all below> \from 'group database';
  assigns \result \from indirect:name[0..], indirect:bufsize;
  assigns *grp \from name[0..], indirect:bufsize;
  assigns buffer[0 .. bufsize-1] \from name[0..], indirect:bufsize;
  assigns *result \from grp;
  assigns errno \from indirect:name[0..], indirect:bufsize;
*/
extern int getgrnam_r(const char *name, struct group *grp, char *buffer,
                      size_t bufsize, struct group **result);

/*@
  //missing: assigns <all below> \from 'group database';
  assigns \result \from &__fc_grp;
  assigns errno \from indirect:__fc_grp;
*/
extern struct group *getgrent(void);

/*@
  //missing: assigns <all below> \from 'group database';
  assigns errno \from indirect:__fc_grp;
*/
extern void endgrent(void);

/*@
  //missing: assigns <all below> \from 'group database';
  assigns errno \from indirect:__fc_grp;
*/
extern void setgrent(void);

/* BSD function */
/*@
  // missing: ... \from groups database
  assigns \result \from indirect:user[0..], indirect:group, indirect:*ngroups;
  assigns groups[0 .. \old(*ngroups) - 1], *ngroups
          \from indirect:user[0..], group, *ngroups;
*/
extern int getgrouplist(const char *user, gid_t group,
                        gid_t *groups, int *ngroups);

/*@
  //missing: assigns <all below> \from 'group database';
  assigns \result \from indirect:user[0..], indirect:group;
  assigns errno \from indirect:user[0..], indirect:group;
*/
extern int initgroups(const char *user, gid_t group);

__END_DECLS

__POP_FC_STDLIB
#endif

