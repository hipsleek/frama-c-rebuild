/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include <unistd.h>

#if defined(_POSIX_C_SOURCE)
long posix_c_source_is = _POSIX_C_SOURCE;
#endif
