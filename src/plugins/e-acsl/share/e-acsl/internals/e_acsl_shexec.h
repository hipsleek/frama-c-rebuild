/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/*! ***********************************************************************
 * \file
 * \brief Interface for running shell commands
 **************************************************************************/

#ifndef E_ACSL_SHEXEC_H
#define E_ACSL_SHEXEC_H

#include "e_acsl_config.h"

// Only available on linux
#if E_ACSL_OS_IS_LINUX

#  include <sys/types.h>

/*! \class ipr_t
 *  \brief Result struct for `shexec` function -- execute a command in the
 *  shell via fork/exec and return results */
typedef struct {
  /** \brief resulting STDERR stream as \p const \p char* */
  char *stderrs;
  /** \brief Supplied STDIN stream as \p const \p char* */
  char *stdins;
  /** \brief resulting STDOUT stream as \p const \p char* */
  char *stdouts;
  /** \brief Exit status of a program */
  int exit_status;
  /** \brief ID of a child process this command has been executed in */
  pid_t pid;
  /** \brief Set to non-zero if child process is interrupted via a signal */
  int signaled;
  /** \brief If \p signalled is set, \p signo is set to the number of signal
   * that interrupted execution of a child process */
  int signo;
  /** \brief A command to execute. Needs to be NULL terminated  */
  char **argv; /** \brief ARGV */
  /** \brief Message if the command has failed to run  */
  char *error;
} ipr_t;

/* \brief Execute a command given via parameter `data` in the current shell
 *  and return the dynamically allocated struct `ipr_t` which captures the
 *  results of the command's execution.
 *
 * \param data - command to execute. `data` is expected to be a NULL-terminated
 *  array of C strings.
 * \param sin - if not NULL, a C string given via `sin` is supplied as standard
 *  input to the executed command.
 * \return - heap-allocated struct `ipr_t` which describes the output of the
 *  executed command. Deallocation of this struct must be performed via the
 *  `free_ipr` function. */
ipr_t *shexec(char **data, const char *sin);

#endif // E_ACSL_OS_IS_LINUX

#endif // E_ACSL_SHEXEC_H
