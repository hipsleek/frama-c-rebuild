/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_TYPES_H
#define __FC_SYS_TYPES_H
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS


#include "../__fc_define_id_t.h"
#include "../__fc_define_pid_t.h"
#include "../__fc_define_size_t.h"
#include "../__fc_define_ssize_t.h"
#include "../__fc_define_uid_and_gid.h"
#include "../__fc_define_time_t.h"
#include "../__fc_define_suseconds_t.h"
#include "../__fc_define_ino_t.h"
#include "../__fc_define_blkcnt_t.h"
#include "../__fc_define_blksize_t.h"
#include "../__fc_define_dev_t.h"
#include "../__fc_define_mode_t.h"
#include "../__fc_define_nlink_t.h"
#include "../__fc_define_off_t.h"
#include "../__fc_define_pthread_types.h"
#include "../__fc_define_key_t.h"

#ifndef __u_char_defined
typedef unsigned long u_long;
typedef unsigned int u_int;
typedef unsigned short u_short;
typedef unsigned char u_char;

// Some glibc versions include major/minor/makedev here, but recently
// they are in 'sysmacros.h'
#include <sys/sysmacros.h>

#define __u_char_defined 1
#endif

// Non-POSIX
#ifndef caddr_t
typedef char *caddr_t;
#endif

__END_DECLS
__POP_FC_STDLIB
#endif
