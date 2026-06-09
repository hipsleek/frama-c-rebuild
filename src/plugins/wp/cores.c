/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include <caml/memory.h>
#include <caml/mlvalues.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

int cores(void) {
#ifdef WIN32
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  return sysinfo.dwNumberOfProcessors;
#else
  return sysconf(_SC_NPROCESSORS_CONF);
#endif
}

CAMLprim value caml_cores(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_int(cores()));
}
