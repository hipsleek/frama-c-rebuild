/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/*! ***********************************************************************
 * \file
 * \brief E-ACSL assertions and abort statements implementation.
 **************************************************************************/

#ifndef E_ACSL_PRIVATE_ASSERT
#define E_ACSL_PRIVATE_ASSERT

/*! \brief Assert with printf-like error message support */
#define private_assert(expr, fmt_and_args...)                                  \
  private_assert_fail(expr, __FILE__, __LINE__, fmt_and_args)

/*! \brief Output a message to error stream using printf-like format string
 * and abort the execution.
 *
 * This is a wrapper for \p eprintf combined with \p abort */
#define private_abort(fmt_and_args...)                                         \
  private_abort_fail(__FILE__, __LINE__, fmt_and_args)

void private_assert_fail(int expr, const char *file, int line, char *fmt, ...);
void private_abort_fail(const char *file, int line, char *fmt, ...);
void raise_abort(const char *file, int line);

/* Instances of assertions shared across different memory models */

/*! \brief Abort the execution if the size of the pointer computed during
 * instrumentation (\p _ptr_sz) does not match the size of the pointer used
 * by a compiler (\p void*) */
#define arch_assert(_ptr_sz)                                                   \
  private_assert(                                                              \
      _ptr_sz == sizeof(void *),                                               \
      "Mismatch of instrumentation- and compile-time pointer sizes: "          \
      "%lu vs %lu\n",                                                          \
      _ptr_sz, sizeof(void *))

#endif // E_ACSL_PRIVATE_ASSERT
