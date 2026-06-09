/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include <limits.h>

#if defined(PATH_MAX)
int path_max_is = PATH_MAX;
#endif

#if defined(TTY_NAME_MAX)
int tty_name_max_is = TTY_NAME_MAX;
#endif

#if defined(HOST_NAME_MAX)
int host_name_max_is = HOST_NAME_MAX;
#endif
