/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "e_acsl_bits.h"

/* Check if we have little-endian and abort the execution otherwise. */
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#  error "Big-endian byte order is unsupported"
#elif __BYTE_ORDER__ == __ORDER_PDP_ENDIAN__
#  error "PDP-endian byte order is unsupported"
#elif __BYTE_ORDER__ != __ORDER_LITTLE_ENDIAN__
#  error "Unknown byte order"
#endif
