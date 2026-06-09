(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Mt_self

let grp_models = add_group "Extraction of models"
let grp_cfg = add_group "Multithreaded control-flow-graph"
let grp_analysis = add_group "Analysis"

module Enabled =
  False (struct
    let option_name = "-mthread"
    let help = "enable analysis of multi-threaded programs through the Mthread plugin"
  end)
;;

let () = Parameter_customize.set_group grp_debug
let () = Parameter_customize.is_invisible ()
module KeepProjects =
  String (struct
    let option_name = "-mt-keep-analyses"
    let help = "keep a copy of the analyses done for each thread"
    let default = "last"
    let arg_name = "all|last|none"
  end)
;;
let () = KeepProjects.set_possible_values ["all"; "last"; "none"]
let () = KeepProjects.add_set_hook
    (fun _old _new ->
       warning "Option -mt-keep-analyses is now deprecated.@ \
                Thread analyses are no longer run in separate projects.")

let () = Parameter_customize.set_group grp_debug
module ToDisk =
  False (struct
    let option_name = "-mt-projects-on-disk"
    let help = "After each thread analysis, save the current analysis state \
                in a separate file"
  end)
;;

let () = Parameter_customize.set_group grp_debug
let () = Parameter_customize.set_negative_option_name "-mt-consider-null"
module IgnoreNull =
  False (struct
    let option_name = "-mt-ignore-null"
    let help = "Ignore shared accesses to numeric memory (NULL base)"
  end)
;;

let () = Parameter_customize.set_group grp_debug
module ToDiskPrefix =
  String
    (struct
      let option_name = "-mt-projects-on-disk-prefix"
      let arg_name = "prefix"
      let default = "mthread_"
      let help = "Prepend <prefix> to the project's filename saved by \
                  -mt-projects-on-disk (defaults to mthread_)"
    end)

let () = Parameter_customize.set_group grp_analysis
module ThreadsLib =
  Enum
    (struct
      type t = Mt_lib.threads_lib
      let option_name = "-mt-threads-lib"
      let help = "Select which threading library is stubbed by MThread. \
                  Defaults to \"builtins-only\"."
      let default = Mt_lib.BuiltinsOnly
      let values = [
        (Mt_lib.BuiltinsOnly, "builtins-only");
        (Mt_lib.Pthreads, "pthreads");
      ]
    end)

let () = Parameter_customize.set_group grp_analysis
module WriteWriteRaces =
  False (struct
    let option_name = "-mt-write-races"
    let help = "Display memory on which there is a write-only race condition"
  end)

let () = Parameter_customize.set_group grp_analysis
module DumpSharedVarsValues =
  Int (struct
    let default = 0
    let option_name = "-mt-shared-values"
    let help = "Show what threads read and write in shared memory at the end of each iteration\n\
                0: values not shown\n\
                1: values shown\n\
                2: values shown with the stack at which the operation occurs"
    let arg_name = "level"
  end)
let () = DumpSharedVarsValues.set_range ~min:0 ~max:2

let () = Parameter_customize.set_group grp_analysis
module CheckProtections =
  False (struct
    let help = "more precise inference of which mutexes protect shared memory"
    let option_name = "-mt-shared-accesses-synchronization"
  end)

let () = Parameter_customize.set_group grp_analysis
module InterruptHandlers =
  Kernel_function_set (struct
    let option_name = "-mt-interrupt-handlers" (* if modified, update name in mt_domain.ml *)
    let arg_name = "functions"
    let help = "Specify functions that will be treated as handlers for \
                interrupts."
  end)

let () = Parameter_customize.set_group messages
module ModerateWarnings =
  True (struct
    let option_name = "-mt-moderate-warnings"
    let help = "Show semi-important warnings during analysis."
  end)

let () = Parameter_customize.set_group messages
module PrintCallstacks  =
  False (struct
    let option_name = "-mt-print-callstacks"
    let help = "Print the callstacks at which concurrent events occur"
  end)

let () = Parameter_customize.set_group grp_debug
module SkipThreads =
  String_set
    (struct
      let option_name = "-mt-skip-threads"
      let arg_name = "th1,...,thn"
      let help = "do not execute the specified threads"
    end)
;;

let () = Parameter_customize.set_group grp_debug
module OnlyThreads =
  String_set
    (struct
      let option_name = "-mt-only-threads"
      let arg_name = "th1,...,thn"
      let help = "only execute the specified threads"
    end)
;;

let () = Parameter_customize.set_group grp_debug
module StopAfter =
  Int (struct
    let default = max_int
    let option_name = "-mt-stop-after"
    let help = "Only perform at most i iterations"
    let arg_name = "i"
  end)
;;

let () = Parameter_customize.set_group grp_debug
module ConcatDotFilesTo =
  Filepath
    (struct
      let option_name = "-mt-concat-dot-files-to"
      let arg_name = "filename"
      let existence = Fclib.Filepath.Indifferent
      let file_kind = "dot"
      let help = "Concatenate dot files generated by the html output into a \
                  single file."
    end)

let () = Parameter_customize.set_group grp_debug
module KeepDotFiles =
  False
    (struct
      let option_name = "-mt-keep-dot-files"
      let help = "Keep dot files used to generate SVG for the html output"
    end)

let () = Parameter_customize.set_group grp_models
module ExtractModels =
  String_set
    (struct
      let option_name = "-mt-extract"
      let arg_name = "[html]"
      let help = "extraction of models"
    end)
;;

let () = Parameter_customize.set_group grp_cfg
module FullCfg =
  False (struct
    let option_name = "-mt-full-cfg"
    let help = "Do not simplify cfg and show all statements (can be costly)"
  end)
;;

let () = Parameter_customize.set_group grp_cfg
module KeepWhiteNodes =
  False (struct
    let option_name = "-mt-non-shared-accesses"
    let help = "Keep accesses to false shared variables in the cfg"
  end)
;;

let () = Parameter_customize.set_group grp_cfg
module KeepGreenNodes =
  True (struct
    let option_name = "-mt-non-concurrent-accesses"
    let help = "Keep non-concurrent accesses to shared variables in the cfg"
  end)
;;

let () = Parameter_customize.set_group grp_cfg
module ShowReturnEdges =
  True (struct
    let option_name = "-mt-return-edges"
    let help = "Show link between a call an a return instruction as a dotted line"
  end)
;;
