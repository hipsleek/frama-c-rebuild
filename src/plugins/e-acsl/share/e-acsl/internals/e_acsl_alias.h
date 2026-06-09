/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/*! ***********************************************************************
 * \file
 * \brief Function aliasing
 **************************************************************************/

#ifndef E_ACSL_ALIAS_H
#define E_ACSL_ALIAS_H

/* Concatenation of 2 tokens */
#define preconcat(x, y) x##y
#define concat(x, y)    preconcat(x, y)
/** Prefix of public functions */
#define export_prefix __e_acsl_
/** Add public prefix to an identifier */
#define export_alias(_n) concat(export_prefix, _n)

#endif
