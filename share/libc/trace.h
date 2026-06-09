/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_TRACE_H
#define __FC_TRACE_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_pid_t.h"
#include "__fc_define_pthread_types.h"
#include "__fc_define_size_t.h"
#include "__fc_define_timespec.h"
#include <limits.h>

__BEGIN_DECLS

typedef unsigned int trace_event_id_t;

typedef unsigned int trace_id_t;

typedef struct __fc_trace_attr_t {
   int _fc ;
} trace_attr_t;

struct posix_trace_event_info {
  trace_event_id_t posix_event_id;
  pid_t posix_pid;
  void *posix_prog_address;
  pthread_t posix_thread_id;
  struct timespec posix_timestamp;
  int posix_truncation_status;
};

typedef struct __fc_trace_event_set_t {
   int _fc_trevset;
} trace_event_set_t;

__FC_EXTERN trace_event_set_t __fc_cur_trace;

struct posix_trace_status_info {
  int posix_stream_full_status;
  int posix_stream_overrun_status;
  int posix_stream_status;
  int posix_log_full_status;
  int posix_log_overrun_status;
  int posix_stream_flush_error;
  int posix_stream_flush_status;
};

// Note: the constants below are not defined in most Linux systems,
// so arbitrary values were chosen.

#define POSIX_TRACE_ALL_EVENTS 1

#define POSIX_TRACE_APPEND 2

#define POSIX_TRACE_CLOSE_FOR_CHILD 3

#define POSIX_TRACE_FILTER 4

#define POSIX_TRACE_FLUSH 5
#define POSIX_TRACE_FLUSH_START 6
#define POSIX_TRACE_FLUSH_STOP 7
#define POSIX_TRACE_FLUSHING 8

#define POSIX_TRACE_FULL 9
#define POSIX_TRACE_LOOP 10
#define POSIX_TRACE_NO_OVERRUN 11

#define POSIX_TRACE_NOT_FLUSHING 12

#define POSIX_TRACE_NOT_FULL 13

#define POSIX_TRACE_INHERITED 14

#define POSIX_TRACE_NOT_TRUNCATED 15
#define POSIX_TRACE_OVERFLOW 16
#define POSIX_TRACE_OVERRUN 17
#define POSIX_TRACE_RESUME 18
#define POSIX_TRACE_RUNNING 19
#define POSIX_TRACE_START 20
#define POSIX_TRACE_STOP 21
#define POSIX_TRACE_SUSPENDED 22
#define POSIX_TRACE_SYSTEM_EVENTS 23
#define POSIX_TRACE_TRUNCATED_READ 24
#define POSIX_TRACE_TRUNCATED_RECORD 25
#define POSIX_TRACE_UNNAMED_USER_EVENT 26
#define POSIX_TRACE_UNTIL_FULL 27
#define POSIX_TRACE_WOPID_EVENTS 28

/*@
  assigns \result, *attr \from *attr;
*/
extern int posix_trace_attr_destroy(trace_attr_t *attr);

/*@
  assigns \result, *resolution \from *attr;
*/
extern int posix_trace_attr_getclockres(const trace_attr_t *attr,
                                        struct timespec *resolution);

/*@
  assigns \result, *createtime \from *attr;
*/
extern int posix_trace_attr_getcreatetime(const trace_attr_t *attr,
                                          struct timespec *createtime);

/*@
  assigns \result, *genversion \from *attr;
*/
extern int posix_trace_attr_getgenversion(const trace_attr_t *attr,
                                          char *genversion);

/*@
  assigns \result, *inheritancepolicy \from *attr;
*/
extern int posix_trace_attr_getinherited(const trace_attr_t *restrict attr,
                                         int *restrict inheritancepolicy);

/*@
  assigns \result, *logpolicy \from *attr;
*/
extern int posix_trace_attr_getlogfullpolicy(const trace_attr_t *restrict attr,
                                             int *restrict logpolicy);

/*@
  assigns \result, *logsize \from *attr;
*/
extern int posix_trace_attr_getlogsize(const trace_attr_t *restrict attr,
                                       size_t *restrict logsize);

/*@
  assigns \result, *maxdatasize \from *attr;
*/
extern int posix_trace_attr_getmaxdatasize(const trace_attr_t *restrict attr,
                                           size_t *restrict maxdatasize);

/*@
  assigns \result, *eventsize \from *attr;
*/
extern int posix_trace_attr_getmaxsystemeventsize(const trace_attr_t *restrict attr,
                                                  size_t *restrict eventsize);

/*@
  assigns \result, *eventsize \from *attr;
*/
extern int posix_trace_attr_getmaxusereventsize(const trace_attr_t *restrict attr,
                                                size_t, size_t *restrict eventsize);

/*@
  assigns \result, tracename[0 .. TRACE_NAME_MAX-1] \from *attr;
*/
extern int posix_trace_attr_getname(const trace_attr_t *attr, char *tracename);

/*@
  assigns \result, *streampolicy \from *attr;
*/
extern int posix_trace_attr_getstreamfullpolicy(const trace_attr_t *restrict attr,
                                                int *restrict streampolicy);

/*@
  assigns \result, *streamsize \from *attr;
*/
extern int posix_trace_attr_getstreamsize(const trace_attr_t *restrict attr,
                                          size_t *restrict streamsize);

/*@
  assigns \result, *attr \from *attr;
*/
extern int posix_trace_attr_init(trace_attr_t *attr);

/*@
  assigns \result, *attr \from inheritancepolicy;
*/
extern int posix_trace_attr_setinherited(trace_attr_t *attr, int inheritancepolicy);

/*@
  assigns \result, *attr \from logpolicy;
*/
extern int posix_trace_attr_setlogfullpolicy(trace_attr_t *attr, int logpolicy);

/*@
  assigns \result, *attr \from logsize;
*/
extern int posix_trace_attr_setlogsize(trace_attr_t *attr, size_t logsize);

/*@
  assigns \result, *attr \from maxdatasize;
*/
extern int posix_trace_attr_setmaxdatasize(trace_attr_t *attr, size_t maxdatasize);

/*@
  assigns \result, *attr \from tracename[0..];
*/
extern int posix_trace_attr_setname(trace_attr_t *attr, const char *tracename);

/*@
  assigns \result, *attr \from streampolicy;
*/
extern int posix_trace_attr_setstreamfullpolicy(trace_attr_t *attr, int streampolicy);

/*@
  assigns \result, *attr \from streamsize;
*/
extern int posix_trace_attr_setstreamsize(trace_attr_t *attr, size_t streamsize);

/*@
  assigns \result \from trid;
*/
extern int posix_trace_clear(trace_id_t trid);

/*@
  assigns \result \from trid;
*/
extern int posix_trace_close(trace_id_t trid);

/*@
  assigns \result, *trid \from *attr, pid; //missing: system clock
*/
extern int posix_trace_create(pid_t pid, const trace_attr_t *restrict attr,
                              trace_id_t *restrict trid);

/*@
  allocates trid;
  assigns \result, *trid \from *attr, pid, file_desc; //missing: system clock
*/
extern int posix_trace_create_withlog(pid_t pid, const trace_attr_t *restrict attr,
                                      int file_desc, trace_id_t *restrict trid);

/*@
  assigns __fc_cur_trace \from __fc_cur_trace, event_id,
                               ((char*)data_ptr)[0 .. data_len - 1];
*/
extern void posix_trace_event(trace_event_id_t event_id,
                              const void *restrict data_ptr, size_t data_len);

/*@
  assigns \result \from indirect:trid, indirect:event1, indirect:event2;
*/
extern int posix_trace_eventid_equal(trace_id_t trid, trace_event_id_t event1,
                                     trace_event_id_t event2);

/*@
  assigns \result, event_name[0 .. TRACE_EVENT_NAME_MAX-1] \from trid, event;
*/
extern int posix_trace_eventid_get_name(trace_id_t trid, trace_event_id_t event,
                                        char *event_name);

/*@
  assigns \result, *event_id \from event_name[0..];
*/
extern int posix_trace_eventid_open(const char *restrict event_name,
                                    trace_event_id_t *restrict event_id);

/*@
  assigns \result, *set \from *set, event_id;
*/
extern int posix_trace_eventset_add(trace_event_id_t event_id, trace_event_set_t *set);

/*@
  assigns \result, *set \from *set, event_id;
*/
extern int posix_trace_eventset_del(trace_event_id_t event_id, trace_event_set_t *set);

/*@
  assigns \result, *set \from *set;
*/
extern int posix_trace_eventset_empty(trace_event_set_t *set);

/*@
  assigns \result, *set \from *set, what;
*/
extern int posix_trace_eventset_fill(trace_event_set_t *set, int what);

/*@
  assigns \result, *ismember \from event_id, *set;
*/
extern int posix_trace_eventset_ismember(trace_event_id_t event_id,
                                         const trace_event_set_t *restrict set,
                                         int *restrict ismember);

/*@
  assigns \result, *event, *unavailable \from trid;
*/
extern int posix_trace_eventtypelist_getnext_id(trace_id_t trid,
                                                trace_event_id_t *restrict event,
                                                int *restrict unavailable);

/*@
  assigns \result \from trid;
*/
extern int posix_trace_eventtypelist_rewind(trace_id_t trid);

/*@
  assigns \result \from trid;
*/
extern int posix_trace_flush(trace_id_t trid);

/*@
  assigns \result, *attr \from trid;
*/
extern int posix_trace_get_attr(trace_id_t trid, trace_attr_t *attr);

/*@
  assigns \result, *set \from trid;
*/
extern int posix_trace_get_filter(trace_id_t trid, trace_event_set_t *set);

/*@
  assigns \result, *statusinfo \from trid;
*/
extern int posix_trace_get_status(trace_id_t trid,
                                  struct posix_trace_status_info *statusinfo);

/*@
  assigns \result, *event, ((char*)data)[0..num_bytes-1], *data_len,
          *unavailable
    \from trid;
*/
extern int posix_trace_getnext_event(trace_id_t trid,
                                     struct posix_trace_event_info *restrict event,
                                     void *restrict data, size_t num_bytes,
                                     size_t *restrict data_len,
                                     int *restrict unavailable);

/*@
  assigns \result, *trid \from file_desc;
*/
extern int posix_trace_open(int file_desc, trace_id_t *trid);

/*@
  assigns \result \from trid;
*/
extern int posix_trace_rewind(trace_id_t trid);

/*@
  assigns \result \from trid, *set, how;
*/
extern int posix_trace_set_filter(trace_id_t trid, const trace_event_set_t *set,
                                  int how);

/*@
  assigns \result \from trid;
*/
extern int posix_trace_shutdown(trace_id_t trid);

/*@
  assigns \result \from trid;
*/
extern int posix_trace_start(trace_id_t trid);

/*@
  assigns \result \from trid;
*/
extern int posix_trace_stop(trace_id_t trid);

/*@
  assigns \result, *event, ((char*)data)[0..num_bytes-1], *data_len, *unavailable
    \from trid, *abstime;
*/
extern int posix_trace_timedgetnext_event(trace_id_t trid,
                                          struct posix_trace_event_info
                                          *restrict event, void *restrict data,
                                          size_t num_bytes,
                                          size_t *restrict data_len,
                                          int *restrict unavailable,
                                          const struct timespec *restrict abstime);

/*@
  assigns \result, *event \from event_name[0..], trid;
*/
extern int posix_trace_trid_eventid_open(trace_id_t trid,
                                         const char *restrict event_name,
                                         trace_event_id_t *restrict event);

/*@
  assigns \result, *event, ((char*)data)[0..num_bytes-1], *data_len,
    *unavailable \from trid;
*/
extern int posix_trace_trygetnext_event(trace_id_t trid,
                                        struct posix_trace_event_info
                                        *restrict event, void *restrict data,
                                        size_t num_bytes,
                                        size_t *restrict data_len,
                                        int *restrict unavailable);

__END_DECLS

__POP_FC_STDLIB
#endif
