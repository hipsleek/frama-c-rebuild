(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*                                                                            *)
(******************************************************************************)

(** adds another preprocessing step in order to expand macros in
    annotations.
*)

(** [file suffix cpp file] takes the file to preprocess,
    and the preprocessing directive, and returns the name of the file
    containing the completely preprocessed source. suffix will be appended
    to the name of intermediate files generated for preprocessing annotations
    (gcc preprocessing differs between .c and .cxx files)

    @raises Sys_error if the file cannot be opened or read.
*)

val file:
  string -> (Filepath.t -> Filepath.t -> string) ->
  Filepath.t -> Filepath.t
