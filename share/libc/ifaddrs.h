/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_IFADDRS_H
#define __FC_IFADDRS_H
#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_define_sockaddr.h"
#include <errno.h>

__BEGIN_DECLS

/* Linux header */
struct ifaddrs {
  struct ifaddrs  *ifa_next;
  char *ifa_name;
  unsigned int ifa_flags;
  struct sockaddr *ifa_addr;
  struct sockaddr *ifa_netmask;
  struct sockaddr *ifa_dstaddr;
  union __fc_ifaddrs_ifa_ifu {
    struct sockaddr *ifu_broadaddr;
    struct sockaddr *ifu_dstaddr;
  } ifa_ifu;
# ifndef ifa_broadaddr
#  define ifa_broadaddr  ifa_ifu.ifu_broadaddr
# endif
# ifndef ifa_dstaddr
#  define ifa_dstaddr    ifa_ifu.ifu_dstaddr
# endif
  void *ifa_data;
};

struct ifmaddrs {
	struct ifmaddrs	*ifma_next;
	struct sockaddr	*ifma_name;
	struct sockaddr	*ifma_addr;
	struct sockaddr	*ifma_lladdr;
};

/*@
  allocates *ifap;
  assigns \result, *ifap, errno \from \nothing;
    // missing: \from 'system interfaces'
*/
extern int getifaddrs(struct ifaddrs **ifap);

/*@
  frees ifa;
  assigns \nothing;
*/
extern void freeifaddrs(struct ifaddrs *ifa);

/*@
  allocates *ifmap;
  assigns \result, *ifmap, errno \from \nothing;
    // missing: \from 'system interfaces'
*/
extern int getifmaddrs(struct ifmaddrs **ifmap);

/*@
  frees ifmp;
  assigns \nothing;
*/
extern void freeifmaddrs(struct ifmaddrs *ifmp);

__END_DECLS

__POP_FC_STDLIB
#endif
