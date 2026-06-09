Parse file without option -mt-threads-lib
  $ frama-c -no-autoload-plugins file.c -save parsed.sav
  [kernel] Parsing file.c (with preprocessing)

Load parsed file and run Mthread: failure as Mthread files are missing.
  $ frama-c -no-autoload-plugins -load-module eva,inout,scope -load parsed.sav -eva -mthread
  [mt] Warning: Mthread is an experimental plugin and is still in development.
  [mt] User Error: Variable "__fc_mthread_threads" not found. It should be in file FRAMAC_SHARE/mt/mthread.c, required for the Mthread analysis. Use parameter -mt-threads-lib to include this file in the parsing phase.
  [kernel] Plug-in mt aborted: invalid user input.
  [1]

Parse file with Mthread builtins only.
  $ frama-c -no-autoload-plugins -load-module eva file.c -save parsed.sav -mt-threads-lib builtins-only
  [mt] Preparing sources for Mthread with builtins only
  [kernel] Parsing FRAMAC_SHARE/mt/mthread.c (with preprocessing)
  [kernel] Parsing file.c (with preprocessing)

Load parsed file and run Mthread: failure as pthreads stubs are missing.
  $ frama-c -no-autoload-plugins -load-module eva,inout,scope -load parsed.sav -eva -mthread
  [mt] Warning: Mthread is an experimental plugin and is still in development.
  [mt] ******* Starting mthread
  [mt] *** Computing value analysis for main thread
  [eva] Analyzing a complete application starting at main
  [eva:initial-state] Values of globals at initialization
    __fc_mthread_threads_running ∈ {0}
    __fc_mthread_threads[0..31] ∈ {0}
    __fc_mthread_mutexes[0..31] ∈ {0}
    __fc_mthread_queues[0..31] ∈ {0}
    x ∈ {0}
    y ∈ {0}
    job1 ∈ {0}
    job2 ∈ {0}
  [mt] New thread: <main>, fun main
  [mt] file.c:18: User Error: 
    Call to pthread_create from the pthreads library, whose Mthread files are missing. Use '-mt-threads-lib pthreads' to enable the support of pthreads, or write a C stub for this function using Mthread primitives.
  [eva] Clean up partial results.
  [kernel] Plug-in mt aborted: invalid user input.
  [1]

Parse file with Mthread builtins and pthreads stubs.
  $ frama-c -no-autoload-plugins -load-module eva file.c -save parsed.sav -mt-threads-lib pthreads
  [mt] Preparing sources for Mthread with lib pthreads
  [kernel] Parsing FRAMAC_SHARE/mt/mthread.c (with preprocessing)
  [kernel] Parsing FRAMAC_SHARE/mt/mthread_pthread.c (with preprocessing)
  [kernel] Parsing file.c (with preprocessing)

Load parsed file and run Mthread, with minimal verbosity.
The analysis succeeds and two alarms are emitted.
  $ frama-c -no-autoload-plugins -load-module eva,inout,scope -load parsed.sav -eva -mthread -eva-verbose 0 -mt-verbose 0
  [eva:experimental] Warning: The mthread domain is experimental.
  [mt] Warning: Mthread is an experimental plugin and is still in development.
  [eva:alarm] file.c:13: Warning: signed overflow. assert x + 1 ≤ 2147483647;
  [eva:alarm] file.c:8: Warning: signed overflow. assert y + 1 ≤ 2147483647;
