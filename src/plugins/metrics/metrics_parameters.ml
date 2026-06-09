(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.Register
    (struct
      let name = "Metrics"
      let shortname = "metrics"
      let help = "syntactic metrics"
    end)

module Enabled =
  False
    (struct
      let option_name = "-metrics"
      let help = "activate metrics computation"
    end)

module ByFunction =
  False
    (struct
      let option_name = "-metrics-by-function"
      let help = "also compute metrics on a per-function basis"
    end)

module OutputFile =
  Filepath
    (struct
      let option_name = "-metrics-output"
      let arg_name = "filename"
      let file_kind = "Text, HTML or JSON"
      let existence = Fclib.Filepath.Indifferent
      let help = "print some metrics into the specified file; \
                  the output format is recognized through the extension: \
                  .text/.txt for text, .html/.htm for HTML, or .json for JSON."
    end)

module ValueCoverage =
  False
    (struct
      let option_name = "-metrics-eva-cover"
      let help = "estimate Eva coverage w.r.t. reachable syntactic definitions"
    end)
let () = ValueCoverage.add_aliases [ "-metrics-value-cover" ]

module AstType =
  String
    (struct
      let option_name = "-metrics-ast"
      let arg_name = "[cabs | cil | acsl]"
      let help = "apply metrics to Cabs or CIL AST, or to ACSL specs"
      let default = "cil"
    end
    )

module Libc =
  False
    (struct
      let option_name = "-metrics-libc"
      let help = "show functions from Frama-C standard C library in the \
                  results; deactivated by default."
    end
    )


let () = AstType.set_possible_values ["cil"; "cabs"; "acsl"]

module SyntacticallyReachable =
  Kernel_function_set
    (struct
      let option_name = "-metrics-cover"
      let arg_name = "f1,..,fn"
      let help = "compute an overapproximation of the functions reachable from \
                  f1,..,fn."
    end
    )

module LocalsSize =
  Kernel_function_set
    (struct
      let option_name = "-metrics-locals-size"
      let arg_name = "f1,...,fn"
      let help = "prints the size of local variables for functions f1,...,fn, \
                  and for the functions called within them \
                  (does not support recursive calls)"
    end)

module UsedFiles =
  False
    (struct
      let option_name = "-metrics-used-files"
      let help = "list files containing global definitions reachable by main"
    end)
