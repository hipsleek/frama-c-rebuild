/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_MMAN_H
#define __FC_SYS_MMAN_H

#include "../features.h"
#include "../__fc_define_mode_t.h"
#include "../__fc_define_off_t.h"
#include "../__fc_define_size_t.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

// The values for the constants below are based on an x86 Linux,
// declared in the order given by POSIX.1-2008.

#define PROT_EXEC  0x4
#define PROT_NONE  0x0
#define PROT_READ  0x1
#define PROT_WRITE 0x2

#define MAP_FIXED   0x10
#define MAP_PRIVATE 0x02
#define MAP_SHARED  0x01

// Non-POSIX, but used in some Linux code, so required for parsing
#define MAP_ANONYMOUS 0x20
#define MAP_ANON MAP_ANONYMOUS
#define MAP_SHARED_VALIDATE 0x03

#define MAP_FAILED ((void*) -1)

#define MS_ASYNC      1
#define MS_INVALIDATE 2
#define MS_SYNC       4

#define MCL_CURRENT 1
#define MCL_FUTURE  2

#define POSIX_MADV_DONTNEED   4
#define POSIX_MADV_NORMAL     0
#define POSIX_MADV_RANDOM     1
#define POSIX_MADV_SEQUENTIAL 2
#define POSIX_MADV_WILLNEED   3

// Not currently defined in any Linux header
//#define POSIX_TYPED_MEM_ALLOCATE
//#define POSIX_TYPED_MEM_ALLOCATE_CONTIG
//#define POSIX_TYPED_MEM_MAP_ALLOCATABLE
//
//struct posix_typed_mem_info {
//  size_t posix_tmi_length;
//}

/*@
  assigns \result \from addr, len;
*/
extern int mlock(const void *addr, size_t len);

/*@
  assigns \result \from flags;
*/
extern int mlockall(int flags);

/*@
  allocates \result;
  assigns \result \from addr, indirect:len, indirect:prot, indirect:flags,
    indirect:fildes, indirect:off;
 */
extern void *mmap(void *addr, size_t len, int prot, int flags,
                  int fildes, off_t off);

/*@
  assigns \result \from addr, len, prot;
*/
extern int mprotect(void *addr, size_t len, int prot);

/*@
  assigns \result \from addr, len, flags;
*/
extern int msync(void *addr, size_t len, int flags);

/*@
  assigns \result \from addr, len;
*/
extern int munlock(const void *addr, size_t len);

/*@
  assigns \result \from \nothing;
*/
extern int munlockall(void);

/*@
  assigns \result \from addr, len;
*/
extern int munmap(void *addr, size_t len);

/*@
  assigns \result \from addr, len, advice;
*/
extern int posix_madvise(void *addr, size_t len, int advice);

// Not currently defined in any Linux header
//int    posix_mem_offset(const void *restrict, size_t, off_t *restrict,
//                        size_t *restrict, int *restrict);
//int    posix_typed_mem_get_info(int, struct posix_typed_mem_info *);
//int    posix_typed_mem_open(const char *, int, int);

/*@
  assigns \result \from name[0..], oflag, mode;
*/
extern int shm_open (const char *name, int oflag, mode_t mode);

/*@
  assigns \result \from name[0..];
*/
extern int shm_unlink (const char *name);

__END_DECLS
__POP_FC_STDLIB
#endif
