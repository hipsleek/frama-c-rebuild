/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/*! ***********************************************************************
 * \file
 * \brief Temporal timestamp retrieval
 **************************************************************************/

#ifndef E_ACSL_TIMESTAMP_RETRIEVAL_H
#define E_ACSL_TIMESTAMP_RETRIEVAL_H

#ifdef E_ACSL_TEMPORAL

#  include <stdint.h>

/*! \brief Return origin time stamp associated with a memory block containing
 * address given by `ptr`. `0` indicates an invalid timestamp, i.e., timestamp
 * of a memory block which does not exist. */
uint32_t origin_timestamp(void *ptr);

/*! \brief Return address of referent shadow */
uintptr_t temporal_referent_shadow(void *addr);

/*! \brief Return referent time stamp associated with a pointer which address
 * is given by `ptr`. This function expects that `ptr` is allocated and at
 * least `sizeof(uintptr_t)` bytes long */
uint32_t referent_timestamp(void *ptr);

/*! \brief Store a referent number `ref` in the shadow of `ptr` */
void store_temporal_referent(void *ptr, uint32_t ref);

#endif // E_ACSL_TEMPORAL

#endif // E_ACSL_TIMESTAMP_RETRIEVAL_H
