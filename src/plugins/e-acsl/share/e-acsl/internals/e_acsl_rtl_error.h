/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/*! ***********************************************************************
 * \file
 * \brief Provide malloc-free replacements for some error formatting
 *        functions.
 **************************************************************************/

#ifndef E_ACSL_RTL_ERROR
#define E_ACSL_RTL_ERROR

#include <errno.h>

/*! \brief `strerror()` replacement without dynamic allocation. */
char *rtl_strerror(int errnum);

/*! \brief `strerror_r()` replacement without dynamic allocation.

    The error message will be copied into `buf` up to `bufsize`. The address of
    the buffer is also returned by the function. */
char *rtl_strerror_r(int errnum, char *buf, size_t bufsize);

#endif // E_ACSL_RTL_ERROR
