/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/*! ***********************************************************************
 * \file
 * \brief Internal defines for E-ACSL set according to the current environment.
 *
 * Instead of using complicated logic with predefined macros in the RTL, the
 * logic should be done in this file and an E-ACSL specific define set to record
 * the result of the logic.
 */

#ifndef E_ACSL_CONFIG_H
#define E_ACSL_CONFIG_H

// OS detection
//  - Assign values to specific OSes
#define E_ACSL_OS_LINUX_VALUE   1
#define E_ACSL_OS_WINDOWS_VALUE 2
#define E_ACSL_OS_OTHER_VALUE   999
//  - Declare defines to test for a specific OS
/*!
 * \brief True if the target OS is linux, false otherwise
 */
#define E_ACSL_OS_IS_LINUX E_ACSL_OS == E_ACSL_OS_LINUX_VALUE
/*!
 * \brief True if the target OS is windows, false otherwise
 */
#define E_ACSL_OS_IS_WINDOWS E_ACSL_OS == E_ACSL_OS_WINDOWS_VALUE
/*!
 * \brief True if the target OS is unknown, false otherwise
 */
#define E_ACSL_OS_IS_OTHER E_ACSL_OS == E_ACSL_OS_OTHER_VALUE
//  - Check current OS
#ifdef __linux__
// Linux compilation
#  define E_ACSL_OS E_ACSL_OS_LINUX_VALUE
#elif defined(WIN32) || defined(_WIN32) || defined(__WIN32)
// Windows compilation
#  define E_ACSL_OS E_ACSL_OS_WINDOWS_VALUE
#else
// Other
#  define E_ACSL_OS E_ACSL_OS_OTHER_VALUE
#  error "Unsupported OS for E-ACSL RTL"
#endif

#endif // E_ACSL_CONFIG_H
