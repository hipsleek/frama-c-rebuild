/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "__fc_builtin.h"
#include "errno.h"
#include "pwd.h"
#include "string.h"

__PUSH_FC_STDLIB

char __fc_getpw_pw_name[64];
char __fc_getpw_pw_passwd[64];
char __fc_getpw_pw_gecos[64];
char __fc_getpw_pw_dir[64];
char __fc_getpw_pw_shell[64];

struct passwd __fc_pwd =
  {.pw_name = __fc_getpw_pw_name,
   .pw_passwd = __fc_getpw_pw_passwd,
   .pw_gecos = __fc_getpw_pw_gecos,
   .pw_dir = __fc_getpw_pw_dir,
   .pw_shell = __fc_getpw_pw_shell};

static int __fc_getpw_init; // used to initialize the __fc_getpw* strings

#define COPY_FIELD(fieldname)                   \
  len = strlen(__fc_pwd.fieldname);             \
  if (remaining <= len) {                       \
    *result = NULL;                             \
    return ERANGE;                              \
  }                                             \
  strcpy(buf, __fc_pwd.fieldname);              \
  buf[len] = 0;                                 \
  pwd->fieldname = buf;                         \
  buf += len + 1;                               \
  remaining -= len

// Common code between getpwman_r and getpwuid_r
/*@ assigns __fc_getpw_init, __fc_getpw_pw_name[0..63],
            __fc_getpw_pw_passwd[0..63], __fc_getpw_pw_gecos[0..63],
            __fc_getpw_pw_dir[0..63], __fc_getpw_pw_shell[0..63],
            buf[0 .. buflen-1], *pwd, *result, \result
      \from __fc_pwd; */
static int __fc_getpw_r(struct passwd *pwd, char *buf,
                        size_t buflen, struct passwd **result) {
  // initialize global strings (only during first execution)
  if (!__fc_getpw_init) {
    __fc_getpw_init = 1;
    Frama_C_make_unknown(__fc_getpw_pw_name, 63);
    __fc_getpw_pw_name[63] = 0;
    Frama_C_make_unknown(__fc_getpw_pw_passwd, 63);
    __fc_getpw_pw_passwd[63] = 0;
    Frama_C_make_unknown(__fc_getpw_pw_gecos, 63);
    __fc_getpw_pw_gecos[63] = 0;
    Frama_C_make_unknown(__fc_getpw_pw_dir, 63);
    __fc_getpw_pw_dir[63] = 0;
    Frama_C_make_unknown(__fc_getpw_pw_shell, 63);
    __fc_getpw_pw_shell[63] = 0;
  }

  // simulate one of several possible errors, or "not found"
  if (Frama_C_interval(0, 1)) {
    // 'error or not found' case
    *result = NULL;
    if (Frama_C_interval(0, 1)) return EIO;
    if (Frama_C_interval(0, 1)) return EMFILE;
    if (Frama_C_interval(0, 1)) return ENFILE;
    // Note: POSIX only specifies these values, but several implementations
    // return others, including ENOENT, EBADF, ESRCH, EWOULDBLOCK, EPERM.
    return 0;
  } else {
    // 'found without errors' case
    size_t len, remaining = buflen;
    // copy string fields to buf
    COPY_FIELD(pw_name);
    COPY_FIELD(pw_passwd);
    COPY_FIELD(pw_gecos);
    COPY_FIELD(pw_dir);
    COPY_FIELD(pw_shell);
    pwd->pw_uid = Frama_C_unsigned_int_interval(0, UINT_MAX);
    pwd->pw_gid = Frama_C_unsigned_int_interval(0, UINT_MAX);
    *result = pwd;
    return 0;
  }
}

int getpwnam_r(const char *name, struct passwd *pwd,
               char *buf, size_t buflen, struct passwd **result) {
  //@ assert valid_read_string(name);
  return __fc_getpw_r(pwd, buf, buflen, result);
}

int getpwuid_r(uid_t uid, struct passwd *pwd,
               char *buf, size_t buflen, struct passwd **result) {
  return __fc_getpw_r(pwd, buf, buflen, result);
}

__POP_FC_STDLIB
