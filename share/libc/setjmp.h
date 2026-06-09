/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SETJMP_H
#define __FC_SETJMP_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

/* Note: setjmp/longjmp/sigsetjmp/siglongjmp are currently unsupported
   by Frama-C and should not be used. This file should nevertheless allow
   Frama-C to parse code using them, but the semantics is unsound. */

// The definitions are inspired from glibc and musl, but simplified.
// Minimally portable code should parse with them.

#include "__fc_define_sigset_t.h"

typedef int __jmp_buf[8]; // arbitrary size

typedef struct __jmp_buf_tag {
  __jmp_buf __jmpbuf;
  int saved;
  sigset_t sigs;
} jmp_buf[1];

typedef jmp_buf sigjmp_buf;

/*@ // unsound - should "assigns \anything"
  assigns \result \from indirect:env; //missing: \from 'value given to longjmp'
*/
extern int setjmp(jmp_buf env);

/*@
 assigns \nothing; //missing: '\result \from setjmp()'
 ensures never_terminates: \false;
*/
extern void longjmp(jmp_buf env, int val);

/*@ // unsound - should "assigns \anything"
  assigns \result \from indirect:env, indirect:savesigs;
  // missing:     \from 'value given to siglongjmp'
*/
extern int sigsetjmp(sigjmp_buf env, int savesigs);

/*@
 assigns \nothing; //missing: '\result \from sigsetjmp()'
 ensures never_terminates: \false;
*/
extern void siglongjmp(sigjmp_buf env, int val);


__END_DECLS

__POP_FC_STDLIB
#endif
