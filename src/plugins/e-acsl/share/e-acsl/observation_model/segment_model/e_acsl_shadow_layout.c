/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include <stddef.h>

#include "../../internals/e_acsl_malloc.h"
#include "../../internals/e_acsl_private_assert.h"
#include "../../internals/e_acsl_rtl_error.h"
#include "../internals/e_acsl_safe_locations.h"
#include "e_acsl_segment_tracking.h"

#include "e_acsl_shadow_layout.h"

#if E_ACSL_OS_IS_LINUX

#  include <asm/prctl.h>
#  include <fcntl.h>
#  include <inttypes.h>
#  include <stdio.h>
#  include <string.h>
#  include <sys/auxv.h>
#  include <sys/resource.h>
#  include <unistd.h>

/** Program stack information {{{ */

/* Symbols exported by the linker script */

/*!\brief The first address past the end of the text segment. */
extern char etext;
/*!\brief The first address past the end of the initialized data segment. */
extern char edata;
/*!\brief The first address past the end of the uninitialized data segment. */
extern char end;
/*!\brief The first address of a program. */
extern char __executable_start;

uintptr_t actual_tls_size = 0;
uintptr_t registered_tls_start = 0;

/* TLS system query structure and functions */
typedef struct {
  long moduleId;
  long tlsoffset;
} TLS_INDEX;

extern uintptr_t __tls_get_addr(TLS_INDEX *idx);
extern void arch_prctl(int mod, uintptr_t *idx);

size_t increase_stack_limit(const size_t size) {
  rlim_t stacksz = (rlim_t)size;
  struct rlimit rl;
  int result = getrlimit(RLIMIT_STACK, &rl);
  if (result == 0) {
    if (rl.rlim_cur < stacksz) {
      if (stacksz > rl.rlim_max)
        stacksz = rl.rlim_max;
      rl.rlim_cur = stacksz;
      result = setrlimit(RLIMIT_STACK, &rl);
      if (result != 0) {
        private_abort("setrlimit: %s \n", rtl_strerror(errno));
      }
    } else {
      stacksz = rl.rlim_cur;
    }
  } else {
    private_abort("getrlimit: %s \n", rtl_strerror(errno));
  }
  return (size_t)stacksz;
}

size_t get_stack_size() {
  struct rlimit rlim;
  private_assert(!getrlimit(RLIMIT_STACK, &rlim),
                 "Cannot detect program's stack size\n", NULL);
  return rlim.rlim_cur;
}

extern char **environ;

/*! \brief Return the greatest (known) address on a program's stack.
 * This function presently determines the address using the address of the
 * last string in `environ`. That is, it assumes that argc and argv are
 * stored below environ, which holds for GCC or Clang and Glibc but is not
 * necessarily true for some other compilers/libraries. */
static uintptr_t get_stack_start(int *argc_ref, char ***argv_ref) {
  char **env = environ;
  while (env[1])
    env++;
  uintptr_t addr = (uintptr_t)*env + strlen(*env);

  /* When returning the end stack address we need to make sure that
   * ::ULONG_BITS past that address are actually writeable. This is
   * to be able to set initialization and read-only bits ::ULONG_BITS
   * at a time. If not respected, this may cause a segfault in
   * ::argv_alloca. */
  uintptr_t stack_end = addr + ULONG_BITS;
  uintptr_t stack_start = stack_end - get_stack_size();

  // Check that the assumption that argc and argv are stored below environ in
  // the stack holds
  if (argc_ref) {
    DVASSERT(stack_start <= (uintptr_t)argc_ref
                 && (uintptr_t)argc_ref <= stack_end,
             "Assumption that argc is stored below environ is not verified.\n"
             "\tStack: [%a - %a]\n"
             "\t&argc: %a\n",
             stack_start, stack_end, argc_ref);
  }
  if (argv_ref) {
    DVASSERT(stack_start <= (uintptr_t)argv_ref
                 && (uintptr_t)argv_ref <= stack_end,
             "Assumption that argv is stored below environ is not verified.\n"
             "\tStack: [%a - %a]\n"
             "\t&argv: %a\n",
             stack_start, stack_end, argv_ref);
  }

  return stack_start;
}
/* }}} */

/** Program global information {{{ */
/*! \brief Return the start address of a segment holding globals (generally
 * BSS and Data segments). */
static uintptr_t get_global_start() {
  return (uintptr_t)&__executable_start;
}

/*! \brief Return byte-size of global segment */
static size_t get_global_size() {
  return ((uintptr_t)&end - get_global_start());
}
/** }}} */

/** Thread-local storage information {{{ */

/*! Thread-local storage (TLS) keeps track of copies of per-thread variables.
 * Even though at the present stage, E-ACSL's RTL is not thread-safe, some
 * of the variables (for instance ::errno) are allocated there. In X86, TLS
 * is typically located somewhere below the program's stack but above mmap
 * areas. TLS is typically separated into two sections: .tdata and .tbss.
 * Similar to globals using .data and .bss, .tdata keeps track of initialized
 * thread-local variables, while .tbss holds uninitialized ones.
 *
 * Start and end addresses of TLS are obtained with system call
 * arch_prctl(FS) return the max address of the TLS segment
 * depending on system alignment policy. Work only on 64 bits system
 *
 * __get_tls_addr return the start address of the TLS segment
 *
 * All __threads variables defined in dynamic libs will not be seen !
 */

/*! \brief Return byte-size of the TLS segment */
inline size_t get_tls_size() {
  return actual_tls_size;
}

static __thread int id_tdata = 1;
static __thread int id_tbss;

/*! \brief Return start address of a program's TLS and compute it's size */
uintptr_t get_tls_start() {
  TLS_INDEX tlsparams = {1, 0};
  uintptr_t tls_start = __tls_get_addr(&tlsparams);
  uintptr_t tls_end;
  arch_prctl(ARCH_GET_FS, &tls_end);
  actual_tls_size = tls_end - tls_start;
  return tls_start;
}

/* }}} */

/** Memory partitions {{{ */
static void init_shadow_layout_global() {
  memory_partition *pglobal = &mem_layout.global;
  set_application_segment(&pglobal->application, get_global_start(),
                          get_global_size(), "global", NULL);
  set_shadow_segment(&pglobal->primary, &pglobal->application, 1,
                     "global_primary");
  set_shadow_segment(&pglobal->secondary, &pglobal->application, 1,
                     "global_secondary");
#  ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&pglobal->temporal_primary, &pglobal->application, 1,
                     "temporal_global_primary");
  set_shadow_segment(&pglobal->temporal_secondary, &pglobal->application, 1,
                     "temporal_global_secondary");
#  endif
}

static void init_shadow_layout_tls() {
  memory_partition *ptls = &mem_layout.tls;

  uintptr_t tls_start =
      get_tls_start(); // don't  call in the set_application_segment function as
                       // get_tls_start compute the tls segment size.
  uintptr_t tls_size = get_tls_size();
  set_application_segment(&ptls->application, tls_start, tls_size, "tls", NULL);
  /* Changes of the ratio in the following would require changes in
   * get_tls_start */
  set_shadow_segment(&ptls->primary, &ptls->application, 1, "tls_primary");
  set_shadow_segment(&ptls->secondary, &ptls->application, 1, "tls_secondary");
#  ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&ptls->temporal_primary, &ptls->application, 1,
                     "temporal_tls_primary");
  set_shadow_segment(&ptls->temporal_secondary, &ptls->application, 1,
                     "temporal_tls_secondary");
#  endif
}

static void init_shadow_layout_vdso() {
  // Retrieve the start address of the VDSO segment
  uintptr_t vdso = getauxval(AT_SYSINFO_EHDR);
  private_assert(
      vdso != 0,
      "Start address of VDSO segment not found in auxiliary vector.\n");

  // Use /proc/self/maps to retrieve the end address of the VDSO segment
  // (using open() instead of fopen() to avoid a dynamic allocation)
  int maps_fd = open("/proc/self/maps", O_RDONLY);
  private_assert(maps_fd >= 0, "Unable to open /proc/self/maps: %s\n",
                 rtl_strerror(errno));

  int result;
  uintptr_t start, end;
  ssize_t count;
  off_t offset;
  char *newline;
  // Buffer to read /proc/self/maps. 255 should be enough to read one line.
  char buffer[255];
  while (1) {
    count = read(maps_fd, buffer, sizeof(buffer) - 1);
    if (count == 0) {
      // If the VDSO segment has not been found, use 0 as start and end
      // addresses
      DVABORT("VDSO segment not found at address %a in /proc/self/maps\n",
              vdso);
      start = 0;
      end = 0;
      break;
    } else if (count < 0) {
      DVABORT("Reading /proc/self/maps failed: %s\n", rtl_strerror(errno));
      break;
    } else {
      // Scan the start and end addresses of the segment
      buffer[count] = '\0';
      result = sscanf(buffer, "%" SCNxPTR "-%" SCNxPTR, &start, &end);
      DVASSERT(result == 2,
               "Scanning for addresses in /proc/self/maps failed, expected 2 "
               "addresses, found: %d, error: %s\n",
               result, rtl_strerror(errno));

      if (start <= vdso && vdso < end) {
        break;
      }
    }
    // Set the file offset to the next line
    do {
      // Look for a newline character
      buffer[count] = '\0';
      newline = strchr(buffer, '\n');
      if (newline != NULL) {
        // Newline character found, set the file offset to the character
        // after the newline
        offset = count - (newline - buffer + 1);
        offset = lseek(maps_fd, -offset, SEEK_CUR);
        DVASSERT(offset != -1,
                 "Unable to move file offset of /proc/self/maps: %s\n",
                 rtl_strerror(errno));
        break;
      } else {
        // No newline found on the current buffer, continue reading the file
        count = read(maps_fd, buffer, sizeof(buffer) - 1);
      }
    } while (count > 0);
  }

  result = close(maps_fd);
  DVASSERT(result == 0, "Unable to close /proc/self/maps: %s\n",
           rtl_strerror(errno));

  // Initialize the memory partition
  memory_partition *pvdso = &mem_layout.vdso;
  set_application_segment(&pvdso->application, start, end - start, "vdso",
                          NULL);
  set_shadow_segment(&pvdso->primary, &pvdso->application, 1, "vdso_primary");
  set_shadow_segment(&pvdso->secondary, &pvdso->application, 1,
                     "vdso_secondary");
#  ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&pvdso->temporal_primary, &pvdso->application, 1,
                     "temporal_vdso_primary");
  set_shadow_segment(&pvdso->temporal_secondary, &pvdso->application, 1,
                     "temporal_vdso_secondary");
#  endif
}

static void init_shadow_layout_stack(int *argc_ref, char ***argv_ref) {
  memory_partition *pstack = &mem_layout.stack;
  set_application_segment(&pstack->application,
                          get_stack_start(argc_ref, argv_ref), get_stack_size(),
                          "stack", NULL);
  /* Changes of the ratio in the following would require changes in
   * get_tls_start */
  set_shadow_segment(&pstack->primary, &pstack->application, 1,
                     "stack_primary");
  set_shadow_segment(&pstack->secondary, &pstack->application, 1,
                     "stack_secondary");
#  ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&pstack->temporal_primary, &pstack->application, 1,
                     "temporal_stack_primary");
  set_shadow_segment(&pstack->temporal_secondary, &pstack->application, 1,
                     "temporal_stack_secondary");
#  endif
}
/** }}} */
#elif E_ACSL_OS_IS_WINDOWS

#  include <dbghelp.h>
#  include <processthreadsapi.h>
#  include <windows.h>

/** Program segment information {{{ */
typedef struct mem_loc_info {
  uintptr_t start;
  size_t size;
} mem_loc_info_t;

static mem_loc_info_t get_section_info(HANDLE hModule,
                                       const char *section_name) {
  // Get the location of the module's IMAGE_NT_HEADERS structure
  IMAGE_NT_HEADERS *pNtHdr = ImageNtHeader(hModule);

  // Section table immediately follows the IMAGE_NT_HEADERS
  IMAGE_SECTION_HEADER *pSectionHdr = (IMAGE_SECTION_HEADER *)(pNtHdr + 1);

  const char *imageBase = (const char *)hModule;
  size_t scnNameSize = sizeof(pSectionHdr->Name);
  char scnName[scnNameSize + 1];
  // Enforce nul-termination for scn names that are the whole length of
  // pSectionHdr->Name[]
  scnName[scnNameSize] = '\0';

  mem_loc_info_t res = {.start = 0, .size = 0};

  for (int scn = 0; scn < pNtHdr->FileHeader.NumberOfSections;
       ++scn, ++pSectionHdr) {
    // Note: pSectionHdr->Name[] is 8-byte long. If the scn name is 8-byte
    // long, ->Name[] will not be nul-terminated. For this reason, copy it to a
    // local buffer that is nul-terminated to be sure we only print the real scn
    // name, and no extra garbage beyond it.
    strncpy(scnName, (const char *)pSectionHdr->Name, scnNameSize);

    if (strcmp(scnName, section_name) == 0) {
      res.start = (uintptr_t)imageBase + pSectionHdr->VirtualAddress;
      res.size = pSectionHdr->Misc.VirtualSize;
      break;
    }
  }

  return res;
}
/** }}} */

/** Program stack information {{{ */
static mem_loc_info_t get_stack_mem_loc_info() {
  ULONG_PTR low;
  ULONG_PTR high;
  GetCurrentThreadStackLimits(&low, &high);
  return (mem_loc_info_t){.start = low, .size = high - low + 1};
}

size_t increase_stack_limit(const size_t size) {
  size_t actual_size = get_stack_size();
  if (actual_size < size) {
    DLOG("Increasing stack size at runtime is unsupported on Windows.\n\
      \t   Actual stack size: %lu\n\
      \tRequested stack size: %lu\n",
         actual_size, size);
  }
  return actual_size;
}

size_t get_stack_size() {
  return get_stack_mem_loc_info().size;
}
/** }}} */

/** Memory partitions {{{ */
static void init_shadow_layout_stack(int *argc_ref, char ***argv_ref) {
  memory_partition *pstack = &mem_layout.stack;
  mem_loc_info_t stack_loc_info = get_stack_mem_loc_info();
  set_application_segment(&pstack->application, stack_loc_info.start,
                          stack_loc_info.size, "stack", NULL);
  set_shadow_segment(&pstack->primary, &pstack->application, 1,
                     "stack_primary");
  set_shadow_segment(&pstack->secondary, &pstack->application, 1,
                     "stack_secondary");
#  ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&pstack->temporal_primary, &pstack->application, 1,
                     "temporal_stack_primary");
  set_shadow_segment(&pstack->temporal_secondary, &pstack->application, 1,
                     "temporal_stack_secondary");
#  endif
}

static void init_shadow_layout_text(HMODULE module) {
  // Retrieve mem loc info for the text section
  mem_loc_info_t text = get_section_info(module, ".text");

  memory_partition *ptext = &mem_layout.text;
  set_application_segment(&ptext->application, text.start, text.size, "text",
                          NULL);
  set_shadow_segment(&ptext->primary, &ptext->application, 1, "text_primary");
  set_shadow_segment(&ptext->secondary, &ptext->application, 1,
                     "text_secondary");
#  ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&ptext->temporal_primary, &ptext->application, 1,
                     "temporal_text_primary");
  set_shadow_segment(&ptext->temporal_secondary, &ptext->application, 1,
                     "temporal_text_secondary");
#  endif
}

static void init_shadow_layout_bss(HMODULE module) {
  // Retrieve mem loc info for the uninidialized data segment
  mem_loc_info_t bss = get_section_info(module, ".bss");

  memory_partition *pbss = &mem_layout.bss;
  set_application_segment(&pbss->application, bss.start, bss.size, "bss", NULL);
  set_shadow_segment(&pbss->primary, &pbss->application, 1, "bss_primary");
  set_shadow_segment(&pbss->secondary, &pbss->application, 1, "bss_secondary");
#  ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&pbss->temporal_primary, &pbss->application, 1,
                     "temporal_bss_primary");
  set_shadow_segment(&pbss->temporal_secondary, &pbss->application, 1,
                     "temporal_bss_secondary");
#  endif
}

static void init_shadow_layout_data(HMODULE module) {
  // Retrieve mem loc info for the initialized data segment
  mem_loc_info_t data = get_section_info(module, ".data");

  memory_partition *pdata = &mem_layout.data;
  set_application_segment(&pdata->application, data.start, data.size, "data",
                          NULL);
  set_shadow_segment(&pdata->primary, &pdata->application, 1, "data_primary");
  set_shadow_segment(&pdata->secondary, &pdata->application, 1,
                     "data_secondary");
#  ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&pdata->temporal_primary, &pdata->application, 1,
                     "temporal_data_primary");
  set_shadow_segment(&pdata->temporal_secondary, &pdata->application, 1,
                     "temporal_data_secondary");
#  endif
}

static void init_shadow_layout_idata(HMODULE module) {
  // Retrieve mem loc info for the initialized data segment
  mem_loc_info_t idata = get_section_info(module, ".idata");

  memory_partition *pidata = &mem_layout.idata;
  set_application_segment(&pidata->application, idata.start, idata.size,
                          "idata", NULL);
  set_shadow_segment(&pidata->primary, &pidata->application, 1,
                     "idata_primary");
  set_shadow_segment(&pidata->secondary, &pidata->application, 1,
                     "idata_secondary");
#  ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&pidata->temporal_primary, &pidata->application, 1,
                     "temporal_idata_primary");
  set_shadow_segment(&pidata->temporal_secondary, &pidata->application, 1,
                     "temporal_idata_secondary");
#  endif
}

static void init_shadow_layout_rdata(HMODULE module) {
  // Retrieve mem loc info for the initialized data segment
  mem_loc_info_t rdata = get_section_info(module, ".rdata");

  memory_partition *prdata = &mem_layout.rdata;
  set_application_segment(&prdata->application, rdata.start, rdata.size,
                          "rdata", NULL);
  set_shadow_segment(&prdata->primary, &prdata->application, 1,
                     "rdata_primary");
  set_shadow_segment(&prdata->secondary, &prdata->application, 1,
                     "rdata_secondary");
#  ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&prdata->temporal_primary, &prdata->application, 1,
                     "temporal_rdata_primary");
  set_shadow_segment(&prdata->temporal_secondary, &prdata->application, 1,
                     "temporal_rdata_secondary");
#  endif
}
/** }}} */
#endif

/** Program heap information {{{ */
static uintptr_t get_heap_start() {
  return mem_spaces.heap_start;
}

size_t get_heap_size() {
  return PGM_HEAP_SIZE;
}

static size_t get_heap_init_size() {
  return get_heap_size() / 8;
}

static void init_shadow_layout_heap() {
  memory_partition *pheap = &mem_layout.heap;
  set_application_segment(&pheap->application, get_heap_start(),
                          get_heap_size(), "heap", mem_spaces.heap_mspace);
  set_shadow_segment(&pheap->primary, &pheap->application, 1, "heap_primary");
  set_shadow_segment(&pheap->secondary, &pheap->application, 8,
                     "heap_secondary");
#ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&pheap->temporal_primary, &pheap->application, 1,
                     "temporal_heap_primary");
  set_shadow_segment(&pheap->temporal_secondary, &pheap->application, 1,
                     "temporal_heap_secondary");
#endif
}
/** }}} */

/** Shadow Layout {{{ */

void set_application_segment(memory_segment *seg, uintptr_t start, size_t size,
                             const char *name, mspace msp) {
  seg->name = name;
  seg->start = start;
  seg->size = size;
  seg->end = seg->start + seg->size - 1;
  seg->mspace = msp;
  seg->parent = NULL;
  seg->shadow_ratio = 0;
  seg->shadow_offset = 0;
}

void set_shadow_segment(memory_segment *seg, memory_segment *parent,
                        size_t ratio, const char *name) {
  seg->parent = parent;
  seg->name = name;
  seg->shadow_ratio = ratio;
  seg->size = parent->size / seg->shadow_ratio;
  seg->mspace = eacsl_create_locked_mspace(seg->size + SHADOW_SEGMENT_PADDING);
  seg->start = (uintptr_t)eacsl_mspace_malloc(seg->mspace, 1);
  seg->end = seg->start + seg->size - 1;
  seg->shadow_offset = parent->start - seg->start;
}

void init_shadow_layout_pre_main() {
  init_shadow_layout_heap();

#if E_ACSL_OS_IS_LINUX
  init_shadow_layout_global();
  init_shadow_layout_vdso();
  init_shadow_layout_tls();
  collect_safe_locations();
#elif E_ACSL_OS_IS_WINDOWS
  HANDLE module = GetModuleHandle(NULL);
  init_shadow_layout_text(module);
  init_shadow_layout_bss(module);
  init_shadow_layout_data(module);
  init_shadow_layout_idata(module);
  init_shadow_layout_rdata(module);
#endif

  mem_layout.is_initialized_pre_main = 1;
}

void init_shadow_layout_main(int *argc_ref, char ***argv_ref) {
  init_shadow_layout_stack(argc_ref, argv_ref);

  mem_layout.is_initialized_main = 1;
}

void clean_shadow_layout() {
  if (mem_layout.is_initialized_pre_main && mem_layout.is_initialized_main) {
    int i;
    for (i = 0; i < sizeof(mem_partitions) / sizeof(memory_partition *); i++) {
      if (mem_partitions[i]->primary.mspace)
        eacsl_destroy_mspace(mem_partitions[i]->primary.mspace);
      if (mem_partitions[i]->secondary.mspace)
        eacsl_destroy_mspace(mem_partitions[i]->secondary.mspace);
    }
  }
}

/** }}} */
