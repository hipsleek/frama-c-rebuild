/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_REGEX_H
#define __FC_REGEX_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_size_t.h"
#include "__fc_define_ssize_t.h"
__BEGIN_DECLS

struct re_pattern_buffer {
  struct re_dfa_t *buffer; // Non-POSIX
  size_t allocated; // Non-POSIX
  char *fastmap; // Non-POSIX
  unsigned char *translate; // Non-POSIX
  size_t re_nsub;
};

typedef struct re_pattern_buffer regex_t;

#define REG_EXTENDED 1
#define REG_ICASE 2
#define REG_NEWLINE 4
#define REG_NOSUB 8


/* Eflags */
#define REG_NOTBOL 1
#define REG_NOTEOL 2

/* Error codes */
typedef enum __fc_reg_errcode_t
{
  REG_NOERROR = 0,
  REG_NOMATCH,	  
  REG_BADPAT,	  
  REG_ECOLLATE,	  
  REG_ECTYPE,	  
  REG_EESCAPE,	  
  REG_ESUBREG,	  
  REG_EBRACK,	  
  REG_EPAREN,	  
  REG_EBRACE,	  
  REG_BADBR,	  
  REG_ERANGE,	  
  REG_ESPACE,	  
  REG_BADRPT,	  
  REG_EEND,	  
  REG_ESIZE,	  
  REG_ERPAREN	  
} reg_errcode_t;

typedef ssize_t regoff_t;

// Non-POSIX
struct re_registers
{
  size_t num_regs;
  regoff_t *start;
  regoff_t *end;
};

typedef struct __fc_regmatch_t
{
  regoff_t rm_so;
  regoff_t rm_eo;
} regmatch_t;

/*@
  allocates preg->buffer, preg->fastmap, preg->translate;
  assigns \result \from indirect:pattern[0..], indirect:cflags;
  assigns *preg \from pattern[0..], cflags;
*/
extern int regcomp(regex_t *restrict preg, const char *restrict pattern, int cflags);

/*@
  assigns \result \from indirect:errcode, indirect:*preg, indirect:errbuf_size;
  assigns errbuf[0 .. errbuf_size-1] \from errcode, *preg,
                                           indirect:errbuf_size;
*/
extern size_t regerror(int errcode, const regex_t *restrict preg,
                       char *restrict errbuf, size_t errbuf_size);

/*@
  assigns \result \from indirect:*preg, indirect:string[0..],
                        indirect:pmatch[0 .. nmatch-1], indirect:eflags;
  assigns pmatch[0 .. nmatch-1] \from *preg, string[0..], indirect:nmatch,
                                      indirect:eflags;
*/
extern int regexec(const regex_t *preg, const char *restrict string,
                   size_t nmatch, regmatch_t pmatch[], int eflags);

/*@
  frees preg->buffer, preg->fastmap, preg->translate;
  assigns \nothing;
*/
extern void regfree(regex_t *preg);

__END_DECLS

__POP_FC_STDLIB
#endif
