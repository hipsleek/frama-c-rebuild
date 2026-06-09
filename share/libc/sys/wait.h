/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_WAIT_H
#define __FC_SYS_WAIT_H
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "../__fc_define_sys_wait_macros.h"

#define WEXITED 4
#define WNOWAIT 0x01000000
#define WSTOPPED 2

#define WCONTINUED 8















#include "../__fc_define_id_t.h"
#include "../__fc_define_pid_t.h"
#include "../__fc_define_uid_and_gid.h"
#include <signal.h>

# ifndef __ENUM_IDTYPE_T
# define __ENUM_IDTYPE_T 1
typedef enum __FC_IDTYPE_T { P_ALL, P_PID, P_PGID } idtype_t;
#endif

/*@ //missing: assigns \result \from 'child processes'
    //missing: terminates 'depending on child processes'
    //missing: may set errno to ECHILD or EINTR
  assigns \result \from \nothing;
  assigns *stat_loc \from \nothing;
  ensures result_ok_or_error: \result == -1 || \result >= 0;
  ensures initialization:stat_loc_init_on_success:
    \result >= 0 && stat_loc != \null ==> \initialized(stat_loc);
  behavior stat_loc_null:
    assumes stat_loc_null: stat_loc == \null;
    assigns \result \from \nothing;
  behavior stat_loc_non_null:
    assumes stat_loc_non_null: stat_loc != \null;
    requires valid_stat_loc: \valid(stat_loc);
    //missing: assigns *stat_loc \from 'child processes'
*/
extern pid_t wait(int *stat_loc);

/*@
  assigns \result, *infop \from idtype, id, options;
*/
extern int waitid(idtype_t idtype, id_t id, siginfo_t *infop, int options);


/*@ //missing: assigns \result \from 'child processes'
    //missing: terminates 'depending on child processes'
    //missing: may set errno to ECHILD, EINTR or EINVAL
  assigns \result \from indirect:options;
  assigns *stat_loc \from indirect:options;
  ensures result_ok_or_error: \result == -1 || \result >= 0;
  ensures initialization:stat_loc_init_on_success:
    \result >= 0 && stat_loc != \null ==> \initialized(stat_loc);
  behavior stat_loc_null:
    assumes stat_loc_null: stat_loc == \null;
    assigns \result \from \nothing;
  behavior stat_loc_non_null:
    assumes stat_loc_non_null: stat_loc != \null;
    requires valid_stat_loc: \valid(stat_loc);
    //missing: assigns *stat_loc \from 'child processes'
*/
extern pid_t waitpid(pid_t pid, int *stat_loc, int options);

#include <sys/resource.h>
// non-POSIX
/*@
  assigns \result, *wstatus, *rusage \from options;
*/
extern pid_t wait3(int *wstatus, int options, struct rusage *rusage);


__END_DECLS
__POP_FC_STDLIB
#endif
