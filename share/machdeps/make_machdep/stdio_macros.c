/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include <stdio.h>

#if defined(BUFSIZ)
int bufsiz_is = BUFSIZ;
#endif
#if defined(EOF)
int eof_is = EOF;
#endif
#if defined(FOPEN_MAX)
int fopen_max_is = FOPEN_MAX;
#endif
#if defined(FILENAME_MAX)
int filename_max_is = FILENAME_MAX;
#endif
#if defined(L_ctermid)
int l_ctermid_is = L_ctermid;
#endif
#if defined(L_tmpnam)
int l_tmpnam_is = L_tmpnam;
#endif
#if defined(TMP_MAX)
int tmp_max_is = TMP_MAX;
#endif
