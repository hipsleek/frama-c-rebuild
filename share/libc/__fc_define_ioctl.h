/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_IOCTL_H
#define __FC_DEFINE_IOCTL_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

// Variadic function: specifications are given below, at each specialized form
extern int ioctl(int, int, ...);

// for Variadic
/*@ assigns \result \from indirect:fd, indirect:request; */
extern int __va_ioctl_void(int fd, int request);

/*@ assigns \result \from indirect:fd, indirect:request, indirect:arg; */
extern int __va_ioctl_int(int fd, int request, int arg);

/*@ assigns \result \from indirect:fd, indirect:request,
      indirect:((char*)argp)[0..];
    assigns ((char*)argp)[0..] \from
      indirect:fd, indirect:request, ((char*)argp)[0..]; */
extern int __va_ioctl_ptr(int fd, int request, void* argp);

__END_DECLS
__POP_FC_STDLIB
#endif
