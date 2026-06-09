/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

_Static_assert((char)-1 >= 0 ? 1 : 0, "char_is_unsigned is False");
_Static_assert((char)-1 >= 0 ? 0 : 1, "char_is_unsigned is True");
