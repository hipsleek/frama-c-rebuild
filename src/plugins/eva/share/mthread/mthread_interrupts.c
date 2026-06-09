/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "mthread_interrupts.h"

__fc_mthread_id __mutex_interrupts;

void __mthread_init_mutex_interrupt() {
  __mutex_interrupts = Frama_C_mutex_init("INTERRUPT");
}

int __FRAMAC_MTHREAD_LOCK_LEVEL = 0;

void __mthread_lock_interrupts() {
  if (__FRAMAC_MTHREAD_LOCK_LEVEL++)
    Frama_C_mthread_show("Overlock INTERRUPT");
  else
    Frama_C_mutex_lock(__mutex_interrupts);
}

void __mthread_unlock_interrupts() {
  if (--__FRAMAC_MTHREAD_LOCK_LEVEL)
    Frama_C_mthread_show("Decreasing INTERRUPT level");
  else
    Frama_C_mutex_unlock(__mutex_interrupts);
}

void __mthread_interrupt(void (*f)(void *), void *arg) {
  while (1) {
    __mthread_lock_interrupts();
    (*f)(arg);
    __mthread_unlock_interrupts();
  }
}

void __mthread_void_interrupt(void (*f)(void)) {
  while (1) {
    __mthread_lock_interrupts();
    (*f)();
    __mthread_unlock_interrupts();
  }
}
