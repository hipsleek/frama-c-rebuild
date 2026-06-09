/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_NDBM_H
#define __FC_NDBM_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_mode_t.h"
#include "__fc_define_size_t.h"

__BEGIN_DECLS

typedef struct __fc_datum {
  void *dptr;
  size_t dsize;
} datum;

typedef int DBM;

#define DBM_INSERT 0
#define DBM_REPLACE 1

volatile DBM __fc_dbm;

/*@
  assigns \result \from indirect:*db;
  assigns *db \from *db;
*/
extern int dbm_clearerr(DBM *db);

/*@
  assigns *db \from *db;
*/
extern void dbm_close(DBM *db);

/*@
  assigns \result \from indirect:*db, indirect:key;
  assigns *db \from *db, key;
*/
extern int dbm_delete(DBM *db, datum key);

/*@
  assigns \result \from indirect:*db;
*/
extern int dbm_error(DBM *db);

/*@
  assigns \result \from *db, key;
  assigns *db \from *db;
*/
extern datum dbm_fetch(DBM *db, datum key);

/*@
  assigns \result \from *db;
  assigns *db \from *db;
*/
extern datum dbm_firstkey(DBM *db);

/*@
  assigns \result \from *db;
  assigns *db \from *db;
*/
extern datum dbm_nextkey(DBM *db);

/*@
  assigns \result \from indirect:file[0..], indirect:open_flags,
    indirect:file_mode;
*/
extern DBM *dbm_open(const char *file, int open_flags, mode_t file_mode);

/*@
  assigns \result \from indirect:*db, indirect:key, indirect:content,
    indirect:store_mode;
  assigns *db \from *db, key, content, store_mode;
*/
extern int dbm_store(DBM *db, datum key, datum content, int store_mode);

__END_DECLS

__POP_FC_STDLIB
#endif
