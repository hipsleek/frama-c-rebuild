/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_TIME_H
#define __FC_TIME_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_clockid_t.h"
#include "__fc_define_null.h"
#include "__fc_define_size_t.h"
#include "__fc_define_time_t.h"
#include "__fc_define_timer_t.h"
#include "__fc_define_timespec.h"
#include "__fc_string_axiomatic.h"
#include <errno.h>
#include <signal.h>

/*
 * Names of the interval timers, and structure
 * defining a timer setting:
 */
#define	ITIMER_REAL		0
#define	ITIMER_VIRTUAL		1
#define	ITIMER_PROF		2


__BEGIN_DECLS

#ifndef __clock_t_defined
typedef unsigned int clock_t;
#define __clock_t_defined 1
#endif
// From POSIX.1-2008: "The value of CLOCKS_PER_SEC shall be 1 million on
// XSI-conformant systems. [...]"
#define CLOCKS_PER_SEC ((time_t)1000000)

struct tm {
  int tm_sec; // seconds after the minute [0, 60]
  int tm_min; // minutes after the hour [0, 59]
  int tm_hour; // hours since midnight [0, 23]
  int tm_mday; // day of the month [1, 31]
  int tm_mon; // months since January [0, 11]
  int tm_year; // years since 1900
  int tm_wday; // days since Sunday [0, 6]
  int tm_yday; // days since January 1 [0, 365]
  int tm_isdst; // Daylight Saving Time flag

  long tm_gmtoff; // Seconds East of UTC
  const char *tm_zone; // Timezone abbreviation
};

struct itimerspec {
  struct timespec  it_interval;
  struct timespec  it_value;
};


// Note: macros and specifications in this file consider that no
// other clocks exist (CLOCK_*_CPUTIME_ID and Linux-specific clocks)
#define CLOCK_REALTIME 0
#define CLOCK_MONOTONIC 1
#define TIMER_ABSTIME 1

// In POSIX, but optional (CPT extension)
#define CLOCK_PROCESS_CPUTIME_ID 2
#define CLOCK_THREAD_CPUTIME_ID 3

// Non-POSIX
#define CLOCK_MONOTONIC_RAW 4
#define CLOCK_REALTIME_COARSE 5
#define CLOCK_MONOTONIC_COARSE 6
#define CLOCK_BOOTTIME 7
#define CLOCK_REALTIME_ALARM 8
#define CLOCK_BOOTTIME_ALARM 9
#define CLOCK_SGI_CYCLE 10
#define CLOCK_TAI 11
#define MAX_CLOCKS 16
#define CLOCKS_MASK (CLOCK_REALTIME | CLOCK_MONOTONIC)
#define CLOCKS_MONO CLOCK_MONOTONIC


__FC_EXTERN unsigned int __fc_time;

/*@ assigns \result \from __fc_time; */
extern clock_t clock(void);

/*@ assigns \result \from time1, time0; */
extern double difftime(time_t time1, time_t time0);

/*@
  requires valid_timeptr: \valid(timeptr);
  assigns *timeptr \from *timeptr;
  assigns \result \from indirect:*timeptr;
 */
extern time_t mktime(struct tm *timeptr);

/*@
  assigns *timer, \result \from __fc_time;
  behavior null:
    assumes timer_null: timer == \null;
    assigns \result \from __fc_time;
  behavior not_null:
    assumes timer_non_null: timer != \null;
    requires valid_timer: \valid(timer);
    assigns *timer, \result \from __fc_time;
    ensures initialization:timer: \initialized(timer);
  complete behaviors;
  disjoint behaviors;
*/
extern time_t time(time_t *timer);

__FC_EXTERN char __fc_ctime[26];

/*@
  requires valid_timeptr: \valid_read(timeptr);
  requires initialization:init_timeptr: \initialized(timeptr);
  assigns __fc_ctime[0..25] \from __fc_ctime[0..25], indirect:*timeptr, indirect:__fc_time;
  assigns \result \from indirect:*timeptr, indirect:__fc_time, &__fc_ctime;
  ensures result_points_to_ctime: \result == &__fc_ctime[0];
  ensures result_valid_string: valid_read_string(&__fc_ctime[0]);
 */
extern char *asctime(const struct tm *timeptr);

/*@
  requires valid_timer: \valid_read(timer);
  requires initialization:init_timer: \initialized(timer);
  assigns __fc_ctime[0..25] \from __fc_ctime[0..25], indirect:*timer, indirect:__fc_time;
  assigns \result \from indirect:*timer, indirect:__fc_time, &__fc_ctime;
  ensures result_points_to_ctime: \result == &__fc_ctime[0];
  ensures result_valid_string: valid_read_string(&__fc_ctime[0]);
*/
extern char *ctime(const time_t *timer);

__FC_EXTERN struct tm __fc_time_tm;

/*@
  requires valid_timer: \valid_read(timer);
  assigns \result \from &__fc_time_tm;
  assigns __fc_time_tm \from *timer;
  ensures result_null_or_internal_tm:
    \result == &__fc_time_tm || \result == \null ;
*/
extern struct tm *gmtime(const time_t *timer);

/*@
  requires valid_timer: \valid_read(timer);
  assigns \result \from &__fc_time_tm;
  assigns __fc_time_tm \from *timer;
  ensures result_null_or_internal_tm:
    \result == &__fc_time_tm || \result == \null;
  ensures maybe_error: errno == \old(errno) || errno == EOVERFLOW;
*/
extern struct tm *localtime(const time_t *timer);

/*@
  requires valid_tm: \valid(tm);
  assigns \result, *tm \from *tm;
  ensures result_err_or_valid: \result == -1 || \result >= 0;
  ensures maybe_error: errno == \old(errno) || errno == EOVERFLOW;
*/
extern time_t timegm(struct tm *tm);

/*@
  requires dst_has_room: \valid(s+(0 .. max-1));
  requires valid_format: valid_read_string(format);
  requires valid_tm: \valid_read(tm);
  assigns s[0 .. max-1] \from indirect:max, indirect:format[0..], indirect:*tm;
  assigns \result \from indirect:max, indirect:format[0..], indirect:*tm;
  ensures result_bounded: \result < max;
  ensures result_valid_string: \result > 0 ==> valid_string(s);
  ensures result_length: \result > 0 ==> s[\result] == 0;
 */
extern size_t strftime(char * restrict s,
                       size_t max,
                       const char * restrict format,
                       const struct tm * restrict tm);

/* POSIX */
/*@
  assigns \result \from buf;
  assigns buf[0..] \from indirect:*tm; //missing: \from 'system clock'
*/
extern char *asctime_r(const struct tm *restrict tm, char *restrict buf);

/*@
  assigns \result \from indirect:clock_id; //missing: \from 'clock'
  assigns *res \from indirect:clock_id; //missing: \from 'clock'
*/
extern int clock_getres(clockid_t clock_id, struct timespec *res);

/*@
  requires tp: \valid(tp);
  assigns \result, *tp, __fc_time \from __fc_time;
  behavior realtime_clock:
    assumes realtime: clk_id == CLOCK_REALTIME;
    ensures success: \result == 0;
    ensures initialization: \initialized(tp);
  behavior monotonic_clock:
    assumes monotonic: clk_id == CLOCK_MONOTONIC;
#ifndef __FC_NO_MONOTONIC_CLOCK
    ensures success: \result == 0;
    ensures initialization: \initialized(tp);
#else
    // simulates a system without monotonic clock
    assigns \result\from clk_id;
    ensures result_error: \result == EINVAL;
#endif
  behavior bad_clock_id:
    assumes bad_id: clk_id != CLOCK_REALTIME && clk_id != CLOCK_MONOTONIC;
    assigns \result \from clk_id;
    ensures result_error: \result == EINVAL;

  complete behaviors;
  disjoint behaviors;
 */
extern int clock_gettime(clockid_t clk_id, struct timespec *tp);

/*@ axiomatic nanosleep_predicates {
  @   predicate abs_clock_in_range{L}(clockid_t id, struct timespec *tm)
  @     reads __fc_time;
  @
  @   predicate valid_clock_id{L}(clockid_t id) // id is a known clock and not
  @     reads __fc_time;                        // the CPU-time clock of the
  @ }                                           // calling thread
*/

/*@ ghost __FC_EXTERN volatile int __fc_interrupted; */

/*@ // missing: may assign to errno: EINTR, EINVAL, ENOTSUP
    // missing: assigns \result, rmtp \from 'clock having id clock_id'
  requires valid_request: \valid_read(rqtp);
  requires initialization:initialized_request:
    \initialized(&rqtp->tv_sec) && \initialized(&rqtp->tv_nsec);
  requires valid_nanosecs: 0 <= rqtp->tv_nsec < 1000000000;
  requires valid_remaining_or_null: rmtp == \null || \valid(rmtp);
  assigns \result \from indirect:__fc_time, indirect:__fc_interrupted,
                        indirect:clock_id, indirect:flags, indirect:rqtp,
                        indirect:*rqtp;
  behavior absolute:
    assumes absolute_time: (flags & TIMER_ABSTIME) != 0;
    assumes no_einval: abs_clock_in_range(clock_id, rqtp) &&
                       valid_clock_id(clock_id);
    assigns \result \from indirect:__fc_time, indirect:__fc_interrupted,
                          indirect:clock_id, indirect:rqtp, indirect:*rqtp;
    ensures result_ok_or_error: \result == 0 || \result == EINTR ||
                                \result == EINVAL || \result == ENOTSUP;
  behavior relative_interrupted:
    assumes relative_time: (flags & TIMER_ABSTIME) == 0;
    assumes interrupted: __fc_interrupted != 0;
    assumes no_einval: valid_clock_id(clock_id);
    assigns \result \from indirect:__fc_time, indirect:clock_id, indirect:rqtp,
                          indirect:*rqtp;
    assigns *rmtp \from __fc_time, indirect:clock_id, indirect:rqtp,
                        indirect:*rqtp, indirect:rmtp;
    ensures result_interrupted: \result == EINTR;
    ensures initialization:interrupted_remaining:
      rmtp != \null ==> \initialized(&rmtp->tv_sec) && \initialized(&rmtp->tv_nsec);
    ensures interrupted_remaining_decreases:
      rmtp != \null ==>
        rqtp->tv_sec * 1000000000 + rqtp->tv_nsec >=
        rmtp->tv_sec * 1000000000 + rmtp->tv_nsec;
    ensures remaining_valid:
      rmtp != \null ==> 0 <= rmtp->tv_nsec < 1000000000;
  behavior relative_no_error:
    assumes relative_time: (flags & TIMER_ABSTIME) == 0;
    assumes not_interrupted: __fc_interrupted == 0;
    assumes no_einval: valid_clock_id(clock_id);
    assigns \result \from indirect:__fc_time, indirect:clock_id,
                          indirect:rqtp, indirect:*rqtp;
    ensures result_ok: \result == 0;
  behavior relative_invalid_clock_id:
    assumes relative_time: (flags & TIMER_ABSTIME) == 0;
    assumes not_interrupted: __fc_interrupted == 0;
    assumes einval: !valid_clock_id(clock_id);
    assigns \result \from indirect:__fc_time, indirect:clock_id,
                          indirect:rqtp, indirect:*rqtp;
    ensures result_einval: \result == EINVAL;
  complete behaviors;
  disjoint behaviors;
*/
extern int clock_nanosleep(clockid_t clock_id, int flags,
                           const struct timespec *rqtp, struct timespec *rmtp);

/*@
  assigns \result \from clock_id, *tp;
*/
extern int clock_settime(clockid_t clock_id, const struct timespec *tp);

/*@
  assigns \result \from buf;
  assigns *buf \from *timep;
*/
extern char *ctime_r(const time_t *timep, char *buf);

extern int getdate_err;

__FC_EXTERN struct tm __fc_getdate;

/*@
  assigns \result \from &__fc_getdate;
  assigns __fc_getdate \from __fc_getdate;
  assigns getdate_err \from string[0..];
*/
extern struct tm *getdate(const char *string);

/*@
  requires valid_timer: \valid_read(timer);
  requires valid_result: \valid(result);
  assigns \result \from indirect:*timer, result;
  assigns *result \from indirect:*timer;
  ensures result_null_or_result:
    \result == result || \result == \null;
*/
extern struct tm *gmtime_r(const time_t *restrict timer,
                           struct tm *restrict result);

/*@
  requires valid_timer: \valid_read(timep);
  requires valid_result: \valid(result);
  assigns \result \from indirect:*timep, result;
  assigns *result \from indirect:*timep;
  assigns errno \from indirect:*timep;
  ensures result_null_or_initialized:initialization:
    (\result == result && \initialized(result)) || \result == \null;
  ensures maybe_error: errno == \old(errno) || errno == EOVERFLOW;
*/
extern struct tm *localtime_r(const time_t *restrict timep,
                              struct tm *restrict result);

/*@ // missing: errno may be set to EINTR (EINVAL prevented by precondition)
  requires valid_request: \valid_read(rqtp);
  requires initialization:initialized_request:
    \initialized(&rqtp->tv_sec) && \initialized(&rqtp->tv_nsec);
  requires valid_nanosecs: 0 <= rqtp->tv_nsec < 1000000000;
  requires valid_remaining_or_null: rmtp == \null || \valid(rmtp);
  assigns \result \from indirect:__fc_time, indirect:rqtp, indirect:*rqtp;
  assigns *rmtp \from indirect:__fc_time, indirect:rqtp, indirect:*rqtp,
                      indirect:rmtp;
  ensures result_elapsed_or_interrupted: \result == 0 || \result == -1;
  ensures initialization:interrupted_remaining:
    rmtp != \null && \result == -1 ==>
      \initialized(&rmtp->tv_sec) && \initialized(&rmtp->tv_nsec);
  ensures interrupted_remaining_decreases:
    rmtp != \null && \result == -1 ==>
      rqtp->tv_sec * 1000000000 + rqtp->tv_nsec >=
      rmtp->tv_sec * 1000000000 + rmtp->tv_nsec;
  ensures interrupted_remaining_valid:
    rmtp != \null && \result == -1 ==> 0 <= rmtp->tv_nsec < 1000000000;
*/
extern int nanosleep(const struct timespec *rqtp, struct timespec *rmtp);

//Note: uncomment functions below when the necessary types will be defined:
// locale_t, timer_t
//extern size_t strftime_l(char *restrict, size_t, const char *restrict,
//                         const struct tm *restrict, locale_t);

/*@
  assigns \result \from buf, indirect:buf[0..], indirect:format[0..];
  assigns *tm \from indirect:buf[0..], indirect:format[0..];
*/
extern char *strptime(const char *restrict buf, const char *restrict format,
                      struct tm *restrict tm);

/*@
  assigns \result, *evp, *timerid \from clockid; //missing: assigns clock
*/
extern int timer_create(clockid_t clockid, struct sigevent *restrict evp,
                        timer_t *restrict timerid);

/*@
  assigns \result \from indirect:timerid; //missing: assigns clock
*/
extern int timer_delete(timer_t timerid);

/*@
  assigns \result \from indirect:timerid;
*/
extern int timer_getoverrun(timer_t timerid);

/*@
  assigns \result, *value \from indirect:timerid;
*/
extern int timer_gettime(timer_t timerid, struct itimerspec *value);

/*@
  assigns \result, *ovalue \from indirect:timerid, flags, *value;
*/
extern int timer_settime(timer_t timerid, int flags,
                         const struct itimerspec *restrict value,
                         struct itimerspec *restrict ovalue);

extern int daylight;
extern long timezone;
extern char *tzname[2];

/*@ assigns tzname[0..1][0..] \from \nothing ;*/
extern void tzset(void);

__END_DECLS

__POP_FC_STDLIB
#endif
