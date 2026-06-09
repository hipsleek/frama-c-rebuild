/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

// Note: this file is non-POSIX, but used by some coreutils.

#ifndef __FC_UTMP_H
#define __FC_UTMP_H
#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_define_pid_t.h"
#include "__fc_utmp_constants.h"
#include <sys/time.h>
#include <stdint.h>

__BEGIN_DECLS

struct lastlog
{
  time_t ll_time;
  char ll_line[UT_LINESIZE];
  char ll_host[UT_HOSTSIZE];
};

struct exit_status
{
  short int e_termination;
  short int e_exit;
};

struct utmp
{
  short int ut_type;
  pid_t ut_pid;
  char ut_line[UT_LINESIZE];
  char ut_id[4];
  char ut_user[UT_NAMESIZE];
  char ut_host[UT_HOSTSIZE];
  struct exit_status ut_exit;
  long int ut_session;
  struct timeval ut_tv;
  int32_t ut_addr_v6[4];
  char __glibc_reserved[20]; // used by who.c
};

/*@
  assigns \result, __fc_fopen[fd] \from __fc_fopen[fd], fd;
*/
extern int login_tty (int fd);

// static storage used by some getter functions
__FC_EXTERN struct utmp __fc_get;

/*@
  assigns __fc_utmp \from __fc_utmp, *ut;
  assigns __fc_wtmp \from __fc_wtmp, *ut;
*/
extern void login (const struct utmp *ut);

/*@
  assigns __fc_utmp \from __fc_wtmp, *ut_line;
  assigns \result \from indirect:__fc_wtmp, indirect:*ut_line;
*/
extern int logout (const char *ut_line);

/*@
  assigns __fc_wtmp \from __fc_wtmp, ut_line[0..], ut_name[0..], ut_host[0..];
*/
extern void logwtmp (const char *ut_line, const char *ut_name,
                     const char *ut_host);

/*@
  assigns \nothing; // missing: assigns 'file named wtmp_file in the filesystem' \from *utmp;
*/
extern void updwtmp (const char *wtmp_file, const struct utmp *utmp);

/*@
  assigns \result \from file[0..]; // missing: assigns 'file named file in the filesystem';
*/
extern int utmpname (const char *file);

/*@
  assigns \result \from &__fc_get, indirect:__fc_utmp;
  assigns __fc_get \from __fc_get, indirect:__fc_utmp;
*/
extern struct utmp *getutent (void);

/*@
  assigns __fc_utmp \from __fc_utmp;
*/
extern void setutent (void);

/*@
  assigns __fc_utmp \from __fc_utmp;
*/
extern void endutent (void);

/*@
  assigns \result \from &__fc_get, indirect:__fc_utmp, indirect:*ut;
  assigns __fc_get \from __fc_get, indirect:__fc_utmp, indirect:*ut;
*/
extern struct utmp *getutid (const struct utmp *ut);

/*@
  assigns \result \from &__fc_get, indirect:__fc_utmp, indirect:*ut;
  assigns __fc_get \from __fc_get, indirect:__fc_utmp, indirect:*ut;
*/
extern struct utmp *getutline (const struct utmp *ut);

/*@
  assigns __fc_utmp \from __fc_utmp, *utmp_ptr;
  assigns \result \from utmp_ptr;
*/
extern struct utmp *pututline (const struct utmp *utmp_ptr);

/*@
  assigns \result \from indirect:__fc_utmp;
  assigns *ubuf \from __fc_utmp;
  assigns *ubufp \from &__fc_utmp;
*/
extern int getutent_r (struct utmp *ubuf, struct utmp **ubufp);

/*@
  assigns \result \from indirect:*ut, indirect:__fc_utmp;
  assigns *ubuf \from indirect:*ut, __fc_utmp;
  assigns *ubufp \from indirect:*ut, &__fc_utmp;
*/
extern int getutid_r (const struct utmp *ut, struct utmp *ubuf,
                      struct utmp **ubufp);

/*@
  assigns \result \from indirect:*ut, indirect:__fc_utmp;
  assigns *ubuf \from indirect:*ut, __fc_utmp;
  assigns *ubufp \from indirect:*ut, &__fc_utmp;
*/
extern int getutline_r (const struct utmp *ut,
                        struct utmp *ubuf, struct utmp **ubufp);


__END_DECLS
__POP_FC_STDLIB
#endif
