(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

(** add a syntactic transformation that will be applied to all freshly parsed
    C files.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
val add_syntactic_transformation: (Cabs.file -> Cabs.file) -> unit

(** the main command to parse a file. Return a thunk that can be used to
    convert the AST to CIL. [original] is the original C file before
    preprocessing, used to print user-friendly filepath in error messages.

    @raise Parse_error if a parsing error occurs
*)
val parse: original:Filepath.t -> Filepath.t -> Cil_types.file * Cabs.file
