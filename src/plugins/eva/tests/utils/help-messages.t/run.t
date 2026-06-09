Help message about Eva abstract domains.
  $ frama-c -no-autoload-plugins -load-module eva,eva.apron -eva-domains help
  [eva] List of available domains:
    cvalue               Main analysis domain, enabled by default. Should not be
                         disabled.
    equality             Infers equalities between syntactic C expressions. Makes
                         the analysis less dependent on temporary variables and
                         intermediate computations.
    symbolic-locations   Infers values of symbolic locations represented by
                         imprecise lvalues, such as t[i] or *p when the possible
                         values of [i] or [p] are imprecise.
    gauges               Infers linear inequalities between the variables
                         modified within a loop and a special loop counter.
    octagon              Infers integer relations of the form b < ±X ± Y < e,
                         where X, Y are program lvalues and b, e are constants.
    multidim             Experimental. Improve the precision over arrays of
                         structures or multidimensional arrays.
    bitwise              Infers bitwise information to interpret more precisely
                         bitwise operators.
    taint                Experimental. Taint analysis
    numerors             Experimental. Infers ranges for the absolute and
                         relative errors in floating-point computations.
    sign                 Infers the sign of program variables.
    apron-box            Experimental. Binding to the apron-box domain of the
                         Apron library. See
                         https://antoinemine.github.io/Apron/doc/ for more
                         details.
    apron-octagon        Experimental. Binding to the apron-octagon domain of the
                         Apron library. See
                         https://antoinemine.github.io/Apron/doc/ for more
                         details.
    apron-polka-equality Experimental. Binding to the apron-polka-equality domain
                         of the Apron library. See
                         https://antoinemine.github.io/Apron/doc/ for more
                         details.
    apron-polka-loose    Experimental. Binding to the apron-polka-loose domain of
                         the Apron library. See
                         https://antoinemine.github.io/Apron/doc/ for more
                         details.
    apron-polka-strict   Experimental. Binding to the apron-polka-strict domain
                         of the Apron library. See
                         https://antoinemine.github.io/Apron/doc/ for more
                         details.
    mthread              Experimental. Domain for the analysis of concurrent
                         programs. Automatically enabled by the -mthread
                         parameter.
    inout                Experimental. Infers the inputs and outputs of each
                         function.
    printer              Debug domain, only useful for developers. Prints the
                         transfer functions used during the analysis.
    traces               Experimental. Builds an over-approximation of all the
                         traces that lead to a statement.

Help message about Eva builtins.
  $ frama-c -eva-builtins-list -no-autoload-plugins -load-module eva,eva.apron
  [eva] List of Eva builtins:
  
  ** Automatic replacements:
     (unless otherwise specified, function <f> is replaced by builtin Frama_C_<f>)
     
     __fc_vla_alloc (replaced by: Frama_C_vla_alloc),
     __fc_vla_free (replaced by: Frama_C_vla_free), acos, acosf, alloca, asin,
     asinf, atan, atan2, atan2f, atanf, calloc, ceil, ceilf, cos, cosf, exp,
     expf, floor, floorf, fmod, fmodf, free, log, log10, log10f, logf, malloc,
     memchr, memcpy, memmove, memset, pow, powf, rawmemchr, realloc,
     reallocarray, round, roundf, sin, sinf, sqrt, sqrtf, strchr, strlen,
     strnlen, trunc, truncf, wcschr, wcslen, wmemchr
  
  ** Full list of builtins (configurable via -eva-builtin):
     
     Frama_C_abstract_cardinal, Frama_C_abstract_max, Frama_C_abstract_min,
     Frama_C_acos, Frama_C_acosf, Frama_C_alloca, Frama_C_asin, Frama_C_asinf,
     Frama_C_assert, Frama_C_atan, Frama_C_atan2, Frama_C_atan2f,
     Frama_C_atanf, Frama_C_builtin_split, Frama_C_builtin_split_all,
     Frama_C_builtin_split_pointer, Frama_C_calloc, Frama_C_ceil,
     Frama_C_ceilf, Frama_C_check_leak, Frama_C_cos, Frama_C_cosf, Frama_C_exp,
     Frama_C_expf, Frama_C_floor, Frama_C_floorf, Frama_C_fmod, Frama_C_fmodf,
     Frama_C_free, Frama_C_interval_split, Frama_C_is_base_aligned,
     Frama_C_log, Frama_C_log10, Frama_C_log10f, Frama_C_logf, Frama_C_malloc,
     Frama_C_memchr, Frama_C_memcpy, Frama_C_memmove, Frama_C_memset,
     Frama_C_mthread_show, Frama_C_mthread_sync, Frama_C_mutex_init,
     Frama_C_mutex_lock, Frama_C_mutex_unlock, Frama_C_offset, Frama_C_pow,
     Frama_C_powf, Frama_C_queue_init, Frama_C_queue_receive,
     Frama_C_queue_send, Frama_C_rawmemchr, Frama_C_realloc,
     Frama_C_reallocarray, Frama_C_round, Frama_C_roundf, Frama_C_sin,
     Frama_C_sinf, Frama_C_sqrt, Frama_C_sqrtf, Frama_C_strchr, Frama_C_strlen,
     Frama_C_strnlen, Frama_C_thread_cancel, Frama_C_thread_create,
     Frama_C_thread_exit, Frama_C_thread_id, Frama_C_thread_priority,
     Frama_C_thread_start, Frama_C_thread_suspend, Frama_C_trunc,
     Frama_C_truncf, Frama_C_ungarble, Frama_C_vla_alloc, Frama_C_vla_free,
     Frama_C_watch_cardinal, Frama_C_watch_value, Frama_C_wcschr,
     Frama_C_wcslen, Frama_C_wmemchr

Help message about message categories.
  $ frama-c -no-autoload-plugins -load-module eva,eva.apron -eva-msg-key help
  [eva] Standard Eva message categories are:
    *                       : All categories
    callstack-hash          : additionally print the current callstack hash in
                              some messages
    callstacks              : print the current callstack alongside some messages
    cardinal                : estimate the number of concrete states approximated
                              by the analysis at the end of each function
    domain_product          : inactive category
    final-states            : at the end of the analysis, print final values
                              inferred at the return point of each analyzed
                              function 
    imprecision             : messages related to possible imprecision of
                              builtins interpreting memcpy, memmove and memset
    include-string-literals : when printing a state, also include globals
                              representing string literals
    initial-state           : at the start of the analysis, print the initial
                              value of global variables
    interferences           : debug messages about interferences from other
                              threads injected in Eva analysis with Mthread
    iterator                : debug messages about the fixpoint engine on the
                              control-flow graph of functions
    malloc                  : messages from the builtins interpreting dynamic
                              allocations
    malloc:automatic-free   : messages emitted when bases are automatically freed
                              (alloca or VLA)
    malloc:new              : messages emitted at the creation of new bases
    nonlin                  : messages about evaluation of subdivisions enabled
                              by -eva-subdivide-non-linear
    partition               : messages about states partitioning
    pointer-comparison      : messages about the evaluation of pointer
                              comparisons
    precision-settings      : messages about the automatic configuration of the
                              analysis by option -eva-precision
    progress                : messages about the analysis progress in the C code
    show                    : show values/states inferred by the analysis on
                              directives such as Frama_C_show_each and
                              Frama_C_dump_each
    split-return            : messages related to option -eva-split-return
    summary                 : print a summary of the analysis at the end,
                              including coverage and alarm numbers
    widen-hints             : debug messages when failing to use widen_hints
                              annotations
    widening                : print a message at each point where the analysis
                              applies a widening
  [eva] Additional message categories for printing domain states on user directives:
    d-apron-box             : print states of the apron-box domain
    d-apron-octagon         : print states of the apron-octagon domain
    d-apron-polka-equality  : print states of the apron-polka-equality domain
    d-apron-polka-loose     : print states of the apron-polka-loose domain
    d-apron-polka-strict    : print states of the apron-polka-strict domain
    d-bitwise               : print states of the bitwise domain
    d-cvalue                : print states of the cvalue domain
    d-equality              : print states of the equality domain
    d-gauges                : print states of the gauges domain
    d-inout                 : print states of the inout domain
    d-mthread               : print states of the mthread domain
    d-multidim              : print states of the multidim domain
    d-numerors              : print states of the numerors domain
    d-octagon               : print states of the octagon domain
    d-printer               : print states of the printer domain
    d-sign                  : print states of the sign domain
    d-symbolic-locations    : print states of the symbolic-locations domain
    d-taint                 : print states of the taint domain
    d-taint-debug           : debug print of the taint domain
    d-traces                : print states of the traces domain
    d-unit                  : print states of the unit domain
  [eva] Message categories by verbosity:
     1: summary
     2: show
     3: malloc:new precision-settings
     4: malloc:automatic-free split-return partition
     5: final-states initial-state
     6: imprecision malloc
     7: widen-hints widening pointer-comparison
     8: nonlin
     9: callstack-hash callstacks
    10: progress
    11: cardinal include-string-literals
  -eva-verbose N automatically enables all message categories with a verbosity equal to or less than N. Default to 5.
