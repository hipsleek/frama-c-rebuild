/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/*! ***********************************************************************
 * \file
 * \brief E-ACSL support of function and statement contracts.
 **************************************************************************/

#ifndef E_ACSL_CONTRACT_H
#define E_ACSL_CONTRACT_H

#include <stddef.h>

#include "../internals/e_acsl_alias.h"

#ifdef __FC_FEATURES_H
#  include <__fc_alloc_axiomatic.h>
#else
/*@ ghost extern int __fc_heap_status; */
#endif

#define contract_t     export_alias(contract_t)
#define contract_init  export_alias(contract_init)
#define contract_clean export_alias(contract_clean)
#define contract_set_behavior_assumes                                          \
  export_alias(contract_set_behavior_assumes)
#define contract_get_behavior_assumes                                          \
  export_alias(contract_get_behavior_assumes)
#define contract_partial_count_behaviors                                       \
  export_alias(contract_partial_count_behaviors)
#define contract_partial_count_all_behaviors                                   \
  export_alias(contract_partial_count_all_behaviors)

/*! \brief Structure to hold pieces of information about function and statement
 * contracts at runtime. */
typedef struct contract_t {
  /*! \internal Number of cells in the char array used to store the results of
     * the assumes clauses.
     */
  size_t char_count;

  /*! \internal Char array to store the results of the assumes clauses. One bit
     * per behavior.
     *
     * The functions \ref find_char_index() and \ref find_bit_index() can be
     * used to find the location of the bit for a specific behavior. */
  char *assumes;
} __attribute__((FC_BUILTIN)) contract_t;

// Use an array of arbitrary length to serve as dynamic allocation base in the
// specifications so that Eva can better interpret contracts in this file.
//@ ghost static contract_t __fc_eacsl_contract_base[INT32_MAX];

/*! \brief Allocate and initialize a structure to hold pieces of information
 * about `size` behaviors.
 *
 * \param size Number of behaviors that the structure should support.
 * \return A structure to hold pieces of information about contracts at runtime.
 */
/*@ allocates \result;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns \result \from &__fc_eacsl_contract_base, indirect:size;
  @ ensures \valid(\result) && \aligned(\result, alignof(contract_t)); */
contract_t *contract_init(size_t size) __attribute__((FC_BUILTIN));

/*! \brief Cleanup the structure `c` previously allocated by
 * \ref contract_init.
 *
 * \param c The structure to deallocate.
 */
/*@ requires \valid(c);
  @ assigns \nothing; */
void contract_clean(contract_t *c) __attribute__((FC_BUILTIN));

/*! \brief Set the result of the assumes clauses for the behavior `i` in the
 * structure.
 *
 * \param c Valid pointer to the structure to update.
 * \param i Index of the behavior. The index must be valid.
 * \param assumes Boolean result of the assumes clauses for the behavior.
 * \see \ref contract_get_behavior_assumes to retrieve the value.
 */
/*@ requires \valid(c);
  @ assigns *c \from indirect:c, indirect:i, assumes; */
void contract_set_behavior_assumes(contract_t *c, size_t i, int assumes)
    __attribute__((FC_BUILTIN));

/*! \brief Retrieve the result of the assumes clauses for the behavior `i` from
 * the structure.
 *
 * \param c Valid pointer to the structure to read.
 * \param i Index of the behavior. The index must be valid.
 * \return The result of the assumes clauses for the behavior `i` (1 for true,
 *         0 for false).
 * \see \ref contract_set_behavior_assumes to set the value.
 */
/*@ requires \valid_read(c);
  @ assigns \result \from indirect:c, indirect:i;
  @ ensures \result == 0 || \result == 1; */
int contract_get_behavior_assumes(const contract_t *c, size_t i)
    __attribute__((FC_BUILTIN));

/*! \brief Count the number of active behaviors among the `count` given
 * behaviors.
 *
 * \param c Valid pointer to the structure to read.
 * \param count Number of behaviors to test. There must be `count` values in
 *              `indexes`.
 * \param ... Indexes of the behaviors to test. The indexes must be valid
 *                and there must be `count` indexes.
 * \return 0 if no behaviors are active, 1 if exactly one behavior is active,
 *         and 2 if more than one behavior is active.
 */
int contract_partial_count_behaviors(const contract_t *c, size_t count, ...)
    __attribute__((FC_BUILTIN));

/*! \brief Count the number of active behaviors among all the behaviors of the
 * contract.
 *
 * \param c Valid pointer to the structure to read.
 * \return 0 if no behaviors are active, 1 if exactly one behavior is active,
 *         and 2 if more than one behavior is active.
 */
int contract_partial_count_all_behaviors(const contract_t *c)
    __attribute__((FC_BUILTIN));

#endif
