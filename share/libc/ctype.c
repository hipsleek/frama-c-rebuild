/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "ctype.h"
#include "__fc_builtin.h"
__PUSH_FC_STDLIB

// This file assumes a simple, C-like locale, with no extra characters
// or special cases.

#define	ISDIGIT(_c) \
	((_c) >= '0' && (_c) <= '9')

#define	ISXDIGIT(_c) \
	(ISDIGIT(_c) || \
	((_c) >= 'a' && (_c) <= 'f') || \
	((_c) >= 'A' && (_c) <= 'F'))

// if locale = "C"
#define	ISLOWER(_c) \
	((_c) >= 'a' && (_c) <= 'z')

// if locale = "C"
#define	ISUPPER(_c) \
	((_c) >= 'A' && (_c) <= 'Z')

#define	ISALPHA(_c) \
	(ISUPPER(_c) || \
	ISLOWER(_c))

#define	ISALNUM(_c) \
	(ISALPHA(_c) || \
	ISDIGIT(_c))

// if locale = "C"
#define	ISSPACE(_c) \
	((_c) == ' ' || \
	(_c) == '\f' || \
	(_c) == '\n' || \
	(_c) == '\r' || \
	(_c) == '\t' || \
	(_c) == '\v' )

// if locale = "C"
#define	ISBLANK(_c) \
	((_c) == ' ' || \
	 (_c) == '\t')

int isalnum(int c) {
  return (ISALNUM(c));
}

int isalpha(int c){
  return (ISALPHA(c));
}

int isblank(int c){
  return (ISBLANK(c)||ISSPACE(c));
}

int iscntrl(int c) {
  return (Frama_C_nondet(0,1));
}

int isdigit(int c) {
  return (ISDIGIT(c));
}

int isgraph(int c) {
  return (Frama_C_nondet(0,1));
}

int islower(int c) {
  return (ISLOWER(c));
}

int isprint(int c) {
  return (Frama_C_nondet(0,1));
}

int ispunct(int c) {
  return (Frama_C_nondet(0,1));
}

int isspace(int c) {
  return (ISSPACE(c));
}

int isupper(int c) {
  return (ISUPPER(c));
}

int isxdigit(int c) {
  return (ISXDIGIT(c));
}

int tolower(int c) {
  if ((c >= 'A') && (c <= 'Z'))
    return c + 0x20;
  return c;
}

int toupper (int c)
{
  if ((c >= 'a') && (c <= 'z'))
    return c - 0x20;
  return c;
}

__POP_FC_STDLIB
