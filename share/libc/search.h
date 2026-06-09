/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SEARCH_H
#define __FC_SEARCH_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_size_t.h"

__BEGIN_DECLS

typedef struct entry {
  char    *key;
  void    *data;
} ENTRY;

typedef enum __fc_search_action { FIND, ENTER } ACTION;
typedef enum __fc_search_visit { preorder, postorder, endorder, leaf } VISIT;

__FC_EXTERN volatile char __fc_search_key[64];
__FC_EXTERN volatile char __fc_search_data[64];

struct entry *__fc_search_tbl; // allocated/freed by hcreate/hdestroy
ENTRY __fc_search_entry = { __fc_search_key, __fc_search_data }; // used by hsearch

/*@
  allocates __fc_search_tbl;
  assigns __fc_search_tbl \from indirect:nel;
  assigns \result \from indirect:nel;
*/
extern int hcreate(size_t nel);

/*@
  frees __fc_search_tbl;
  assigns __fc_search_tbl \from \nothing;
*/
extern void hdestroy(void);

/*@
  assigns \result \from &__fc_search_entry, indirect:item,
    indirect:action;
  assigns __fc_search_key[0..] \from __fc_search_key[0..], item.key[0..],
    indirect:action;
  assigns __fc_search_data[0..] \from __fc_search_data[0..],
    ((char*)item.data)[0..], indirect:action;
  ensures result_points_to_dummy_data: \result == &__fc_search_entry;
*/
extern ENTRY *hsearch(ENTRY item, ACTION action);

/*@
  assigns ((char*)element)[0..], ((char*)pred)[0..]
    \from ((char*)element)[0..], ((char*)pred)[0..];
*/
extern void insque(void *element, void *pred);

/*@
  assigns \result \from base, indirect:key, indirect:*nelp, indirect:width,
    indirect:compar;
*/
extern void *lfind(const void *key, const void *base, size_t *nelp,
                   size_t width, int (*compar)(const void *, const void *));

/*@
  assigns \result \from base, indirect:key, indirect:*nelp, indirect:width,
    indirect:compar;
  assigns ((char*)base)[0..] \from ((char*)base)[0..],
    indirect:((char*)key)[0..], indirect:*nelp, indirect:width, indirect:compar;
  assigns *nelp \from *nelp, indirect:((char*)key)[0..],
    indirect:((char*)base)[0..], indirect:compar;
*/
extern void *lsearch(const void *key, void *base, size_t *nelp,
                     size_t width, int (*compar)(const void *, const void *));

/*@
  assigns ((char*)element)[0..] \from ((char*)element)[0..];
*/
extern void remque(void *element);

/*@
  frees *rootp;
  assigns ((char*)rootp)[0..] \from ((char*)key)[0..], ((char*)rootp)[0..],
    indirect:compar;
  assigns \result \from *rootp, indirect:((char*)key)[0..], indirect:compar;
*/
extern void *tdelete(const void *restrict key, void **restrict rootp,
                     int(*compar)(const void *, const void *));

/*@
  assigns \result \from *rootp, indirect:((char*)key)[0..], indirect:compar;
*/
extern void *tfind(const void *key, void *const *rootp,
                   int(*compar)(const void *, const void *));

/*@
  allocates \result;
  allocates *rootp;
  assigns *rootp \from *rootp, indirect:((char*)key)[0..],
    indirect:((char*)rootp)[0..], indirect:compar;
  assigns \result \from *rootp, indirect:((char*)key)[0..],
    indirect:((char*)rootp)[0..], indirect:compar;
*/
extern void *tsearch(const void *key, void **rootp,
                     int(*compar)(const void *, const void *));

/*@
  assigns \nothing;
*/
extern void twalk(const void *root,
                  void (*action)(const void *, VISIT, int ));

__END_DECLS

__POP_FC_STDLIB
#endif
