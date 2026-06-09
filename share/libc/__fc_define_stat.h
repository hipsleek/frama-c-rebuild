/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_STAT_H
#define __FC_DEFINE_STAT_H
#include "features.h"
__PUSH_FC_STDLIB


#include "__fc_define_ino_t.h"
#include "__fc_define_uid_and_gid.h"
#include "__fc_define_time_t.h"
#include "__fc_define_blkcnt_t.h"
#include "__fc_define_blksize_t.h"
#include "__fc_define_dev_t.h"
#include "__fc_define_mode_t.h"
#include "__fc_define_nlink_t.h"
#include "__fc_define_off_t.h"
#include "__fc_define_timespec.h"

#define __statfs_word unsigned int

__BEGIN_DECLS

struct statfs {
	__statfs_word f_type;
	__statfs_word f_bsize;
	__statfs_word f_blocks;
	__statfs_word f_bfree;
	__statfs_word f_bavail;
	__statfs_word f_files;
	__statfs_word f_ffree;
	__statfs_word  f_fsid;
	__statfs_word f_namelen;
	__statfs_word f_frsize;
	__statfs_word f_flags;
	__statfs_word f_spare[4];
};

struct stat {
  dev_t     st_dev;
  ino_t     st_ino;
  mode_t    st_mode;
  nlink_t   st_nlink;
  uid_t     st_uid;
  gid_t     st_gid;
  dev_t     st_rdev;
  off_t     st_size;
  struct timespec    st_atim;
#define st_atime st_atim.tv_sec
  struct timespec    st_mtim;
#define st_mtime st_mtim.tv_sec
  struct timespec    st_ctim;
#define st_ctime st_ctim.tv_sec
  blksize_t st_blksize;
  blkcnt_t  st_blocks;
};

__END_DECLS

__POP_FC_STDLIB
#endif
