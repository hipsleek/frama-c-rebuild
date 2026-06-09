/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_ICONV_H
#define __FC_ICONV_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_size_t.h"
#include <errno.h>

__BEGIN_DECLS

typedef void * iconv_t;

/*@ assigns *outbuf[0 .. *outbytesleft-1] \from *inbuf[0 .. *inbytesleft-1];
  assigns __fc_errno ; */
extern size_t  iconv(iconv_t cd, char **restrict inbuf, size_t *restrict inbytesleft,
            char **restrict outbuf, size_t *restrict outbytesleft);

/*@ assigns __fc_errno;
  ensures result_zero_or_neg: \result == 0 || \result == -1 ; */
extern int     iconv_close(iconv_t);

/*@ assigns \result \from tocode[..],fromcode[..];
  assigns __fc_errno; */
extern iconv_t iconv_open(const char *tocode, const char *fromcode);

__END_DECLS

__POP_FC_STDLIB
#endif
