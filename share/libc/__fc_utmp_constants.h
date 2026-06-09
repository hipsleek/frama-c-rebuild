/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_UTMP_CONSTANTS_H
#define __FC_UTMP_CONSTANTS_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#include <stdio.h>

#define _PATH_UTMP "/var/run/utmp"
#define UTMP_FILE     _PATH_UTMP
#define UTMP_FILENAME _PATH_UTMP
#define _PATH_WTMP "/var/log/wtmp"
#define WTMP_FILE     _PATH_WTMP
#define WTMP_FILENAME _PATH_WTMP

#define UT_LINESIZE  32
#define UT_NAMESIZE  32
#define UT_HOSTSIZE  256

#define EMPTY 0
#define RUN_LVL 1
#define BOOT_TIME 2
#define NEW_TIME 3
#define OLD_TIME 4
#define INIT_PROCESS 5
#define LOGIN_PROCESS 6
#define USER_PROCESS 7
#define DEAD_PROCESS 8
#define ACCOUNTING 9

#define ut_name ut_user
#ifndef _NO_UT_TIME
# define ut_time ut_tv.tv_sec
#endif
#define ut_xtime ut_tv.tv_sec
#define ut_addr ut_addr_v6[0]

// represents the user accounting database, /var/run/utmp
__FC_EXTERN FILE __fc_utmp;

// represents the user accounting database, /var/run/wtmp
__FC_EXTERN FILE __fc_wtmp;

__END_DECLS
__POP_FC_STDLIB
#endif
