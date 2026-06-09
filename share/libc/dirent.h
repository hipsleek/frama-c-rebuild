/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DIRENT_H
#define __FC_DIRENT_H
#include "features.h"
__PUSH_FC_STDLIB

#include <errno.h>

#include "__fc_define_ino_t.h"
#include "__fc_define_off_t.h"

__BEGIN_DECLS

struct dirent {
    ino_t          d_ino;       /* inode number */
    off_t          d_off;       /* offset to the next dirent */
    unsigned short d_reclen;    /* length of this record */
    unsigned char  d_type;      /* type of file; not supported
                                   by all file system types */
    char           d_name[256]; /* filename */
};

typedef struct DIR {
  unsigned int __fc_dir_id;
  unsigned int __fc_dir_position;
  struct stat* __fc_dir_inode;
  struct dirent ** __fc_dir_entries;
} DIR;

__FC_EXTERN DIR __fc_opendir[__FC_FOPEN_MAX];

/*@
  assigns \result \from indirect:*a[0..], indirect:*b[0..];
*/
extern int alphasort(const struct dirent **a, const struct dirent **b);

/*@
  requires dirp_valid_dir_stream: \subset(dirp,&__fc_opendir[0 .. __FC_FOPEN_MAX-1]);
  assigns \result, errno \from indirect:dirp, indirect:*dirp,
                               indirect:__fc_opendir[0 .. __FC_FOPEN_MAX-1];
  assigns *dirp \from indirect:dirp, *dirp, __fc_opendir[0 .. __FC_FOPEN_MAX-1];
  ensures err_or_closed_on_success:
    (\result == 0 && dirp->__fc_dir_inode == \null) || \result == -1;
*/
extern int closedir(DIR *dirp);

/*@
  assigns \result \from *fd;
*/
extern int dirfd(DIR *fd);

/*@
  assigns \result \from &__fc_opendir;
  assigns errno \from indirect:__fc_opendir[0 .. __FC_FOPEN_MAX-1];
  assigns __fc_opendir[0 .. __FC_FOPEN_MAX-1] \from __fc_opendir[0 .. __FC_FOPEN_MAX-1];
*/
extern DIR *fdopendir(int fd);

/*@
  assigns \result \from path[0..], &__fc_opendir;
  assigns errno \from indirect:path[0..], indirect:&__fc_opendir;
  ensures result_null_or_valid: \result == \null || \valid(\result);
  ensures valid_dir_stream_on_success:
    \result != \null ==> \result == &__fc_opendir[\result->__fc_dir_id];
  ensures stream_positioned_on_success:
    \result != \null ==> \result->__fc_dir_inode != \null;
*/
extern DIR *opendir(const char *path);

/*@
  requires dirp_valid_dir_stream: \subset(dirp, &__fc_opendir[0 .. __FC_FOPEN_MAX-1]);
  assigns \result \from *dirp, __fc_opendir[0 .. __FC_FOPEN_MAX-1];
  assigns dirp->__fc_dir_position \from dirp->__fc_dir_position;
  assigns errno \from indirect:dirp, indirect:*dirp,
                      indirect:__fc_opendir[0 .. __FC_FOPEN_MAX-1];
  ensures result_null_or_valid: \result == \null || \valid(\result);
*/
extern struct dirent *readdir(DIR *dirp);

/*@
  assigns \result, errno \from indirect:*dirp, indirect:__fc_opendir[0 .. __FC_FOPEN_MAX-1];
  assigns *dirp, *entry \from *dirp;
  assigns *result \from entry;
*/
extern int readdir_r(DIR * dirp, struct dirent * entry,
                     struct dirent ** result);

/*@
  assigns *dirp \from *dirp;
*/
extern void rewinddir(DIR *dirp);

/*@
  allocates *namelist;
  assigns \result, *namelist
    \from indirect:dir[0..], indirect:sel, indirect:compar;
*/
extern int scandir(const char *dir, struct dirent ***namelist,
                   int (*sel)(const struct dirent *),
                   int (*compar)(const struct dirent **,
                                 const struct dirent **));

/*@
  assigns *dirp \from *dirp, loc;
*/
extern void seekdir(DIR *dirp, long loc);

/*@
  assigns \result \from *dirp;
*/
extern long telldir(DIR *dirp);



/* File types for `d_type'.  */
enum __fc_readdir_dtype
  {
    DT_UNKNOWN = 0,
# define DT_UNKNOWN	DT_UNKNOWN
    DT_FIFO = 1,
# define DT_FIFO	DT_FIFO
    DT_CHR = 2,
# define DT_CHR		DT_CHR
    DT_DIR = 4,
# define DT_DIR		DT_DIR
    DT_BLK = 6,
# define DT_BLK		DT_BLK
    DT_REG = 8,
# define DT_REG		DT_REG
    DT_LNK = 10,
# define DT_LNK		DT_LNK
    DT_SOCK = 12,
# define DT_SOCK	DT_SOCK
    DT_WHT = 14
# define DT_WHT		DT_WHT
  };

/* Convert between stat structure types and directory types.  */
# define IFTODT(mode)	(((mode) & 0170000) >> 12)
# define DTTOIF(dirtype)	((dirtype) << 12)

__END_DECLS

__POP_FC_STDLIB
#endif

