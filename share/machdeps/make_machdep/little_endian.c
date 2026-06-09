/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#if defined(__BYTE_ORDER__)
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
_Static_assert(0, "little_endian is False");
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
_Static_assert(0, "little_endian is True");
#else
#error Unexpected __BYTE_ORDER__
#endif
#else
#error __BYTE_ORDER__ undefined
#endif
