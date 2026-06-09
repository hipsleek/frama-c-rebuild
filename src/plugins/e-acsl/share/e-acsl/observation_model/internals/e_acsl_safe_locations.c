/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include <errno.h>
#include <limits.h>
#include <stdio.h>

#include "../../internals/e_acsl_config.h"
#include "e_acsl_safe_locations.h"

/* An array storing safe locations up to `loc_count` position.
 * This array should be initialized via a below function called
 * `collect_safe_locations`. */
static __thread memory_location locs[16];
static __thread int loc_count = 0;

// These variables allow for an optimisation of get_safe_location.
static __thread uintptr_t min_addr = __UINTPTR_MAX__;
static __thread uintptr_t max_addr = 0;

#define add_safe_location(_name, _addr, _len, _init, _writeable, _freeable)    \
  do {                                                                         \
    min_addr = min_addr <= _addr ? min_addr : _addr;                           \
    max_addr = max_addr >= _addr + _len ? max_addr : _addr + _len;             \
    locs[loc_count].name = _name;                                              \
    locs[loc_count].address = _addr;                                           \
    locs[loc_count].length = _len;                                             \
    locs[loc_count].initialized = _init;                                       \
    locs[loc_count].writeable = _writeable;                                    \
    locs[loc_count].freeable = _freeable;                                      \
    loc_count++;                                                               \
  } while (0)

void collect_safe_locations() {
  /* Tracking of errno and standard streams */
  add_safe_location("&errno", (uintptr_t)&errno, sizeof(int), 1, 1, 0);
  add_safe_location("stdout", (uintptr_t)stdout, sizeof(FILE), 1, 1, 0);
  add_safe_location("stderr", (uintptr_t)stderr, sizeof(FILE), 1, 1, 0);
  add_safe_location("stdin", (uintptr_t)stdin, sizeof(FILE), 1, 0, 0);
}

memory_location *get_safe_location(uintptr_t addr, long size) {
  if (addr < min_addr || addr > max_addr) // covers virtually all cases
    return NULL;
  for (int i = 0; i < loc_count; i++) {
    memory_location safeloc = locs[i];
    if (addr >= safeloc.address
        && addr + size <= safeloc.address + safeloc.length)
      return &locs[i];
  }
  return NULL;
}
