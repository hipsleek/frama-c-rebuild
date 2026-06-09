/***************************************************************************/
/*                                                                         */
/*  SPDX-License-Identifier BSD-3-Clause                                   */
/*  Copyright (c) 2004,2012 Kustaa Nyholm / SpareTimeLabs                  */
/*  All rights reserved.                                                   */
/*  Modified by                                                            */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives).  */
/*                                                                         */
/***************************************************************************/

/*! ***********************************************************************
 * \file
 * \brief Malloc and stdio free implementation printf.
 *
 * Supported format strings:
 * - Flag characters:
 *     - 0       - the following value will be is zero-padded.
 *
 * - Field width:
 *     - Optional positive decimal integer following flag characters.
 *
 * - Length modifier:
 *     - l       - the following integer conversion corresponds to a long int or
 *                  unsigned long int argument.
 *
 * - Standard conversion specifiers:
 *    - d       - signed integers.
 *    - u       - unsigned integers.
 *    - f       - floating point numbers. Floating point numbers do not support
 *    -             precision specification.
 *    - x,X     - hexadecimal numbers.
 *    - p       - void pointers.
 *
 * - Non-standard conversion specifiers:
 *     - a       - memory-address.
 *     - b, B    - print field width bits of a number left-to-right (b) or
 *      right-to-left (B). Unless specified field-width of 8 is used. Bits
 *      over a 64-bit boundary are ignored.
 *     - v, V    - print first field width bits of a memory region given by a
 *      void pointer left-to-right (v) or right-to-left (V). Unless specified
 *      field-width of 8 is used.
 **************************************************************************/

#ifndef E_ACSL_RTL_IO_H
#define E_ACSL_RTL_IO_H

#include <stdarg.h>

#define STDOUT(...) rtl_printf(__VA_ARGS__)
#define STDERR(...) rtl_eprintf(__VA_ARGS__)

#ifdef E_ACSL_CONCURRENCY_PTHREAD
#  include <pthread.h>

/*! \brief Returns the global mutex for RTL printing. This mutex is recursive so
    multiple locks can be acquired from the same thread. */
pthread_mutex_t *rtl_io_global_mutex();

/*! \brief Lock the global RTL printing mutex. */
#  define RTL_IO_LOCK() pthread_mutex_lock(rtl_io_global_mutex());

/*! \brief Unlock the global RTL printing mutex. */
#  define RTL_IO_UNLOCK() pthread_mutex_unlock(rtl_io_global_mutex());
#else
#  define RTL_IO_LOCK()
#  define RTL_IO_UNLOCK()
#endif

/* Replacement for printf with support for the above specifiers */
int rtl_printf(char *fmt, ...);
int rtl_vprintf(char *fmt, va_list vlist);

/* Same as printf but write to a string buffer */
int rtl_sprintf(char *s, char *fmt, ...);
int rtl_vsprintf(char *s, char *fmt, va_list vlist);

/* Same as sprintf buf write only up to ̀bufsize - 1` characters in the buffer
   and the last character is always a `\0`.
   This function is more secure than `sprintf()` because if the buffer `s` is
   valid for `bufsize` characters, it guarantees that there will not be an out
   of bound write. This function is used for instance in `private_assert_fail()`
   or in `rtl_strerror()`. */
int rtl_snprintf(char *s, size_t bufsize, char *fmt, ...);
int rtl_vsnprintf(char *s, size_t bufsize, char *fmt, va_list vlist);

/* Same as printf but write to the error stream. */
int rtl_eprintf(char *fmt, ...);
int rtl_veprintf(char *fmt, va_list vlist);

/* Same as printf but write to a file descriptor. */
int rtl_dprintf(int fd, char *fmt, ...);
int rtl_vdprintf(int fd, char *fmt, va_list vlist);

int rtl_vformat_stderr();

#endif // E_ACSL_RTL_IO_H
