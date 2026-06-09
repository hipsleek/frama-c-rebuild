/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include <signal.h>

#if defined(NSIG)
int nsig_is = NSIG;
#elif defined(_NSIG)
int nsig_is = _NSIG;
#endif
