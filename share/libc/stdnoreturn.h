/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

// This file is not in the C standard; it exists for compatibility purposes

#ifndef __FC_STDNORETURN_H
#define __FC_STDNORETURN_H

// 'noreturn' is an attribute in C++
#ifndef __cpluscplus
#define noreturn _Noreturn
#endif

#endif
