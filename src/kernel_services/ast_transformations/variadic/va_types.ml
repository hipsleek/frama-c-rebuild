(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types


type variadic_class =
  | Unknown
  (** Function declared and not known by Frama-C *)
  | Builtin
  (** Function registered as a builtin function in Cil_builtins *)
  | Defined
  (** Function for which we have the definition in the project *)
  | Misc
  (** Function from the Frama-C lib *)
  | Overload of overload
  (** Function from the Frama-C lib which declines into a finite number of
      possible prototypes whose names are given in the list *)
  | Aggregator of aggregator
  (** Function from the Frama-C lib which has a not-variadic equivalent with
      the variadic part replaced by an array. (The array is the aggregation of
      the arguments from the variadic part. *)
  | FormatFun of format_fun
  (** Function from the Frama-C lib for which the argument count and type is
      fixed by a format argument. *)
  | NoTranslation
  (** Function that should not be translated. *)

and overload = (typ list * varinfo) list

and aggregator = {
  a_target: varinfo;
  a_pos: int;
  a_type: aggregator_type;
  a_param: string * typ;
}

and aggregator_type = EndedByNull

and format_fun = {
  f_kind : Format_types.format_kind;
  f_buffer  : buffer;
  f_format_pos : int;
}

and buffer =
  | StdIO (** Standard input/output (stdin/stdout/stderr) *)
  | Arg of int * int option (* Position of the buffer and size arguments *)
  | Stream of int (* Position of the stream argument *)
  | File of int  (* Position of the file argument *)
  | Syslog (* Output to some system log *)


type variadic_function = {
  vf_decl: varinfo;
  vf_original_type: typ;
  vf_class: variadic_class;
  mutable vf_specialization_count: int; (* The number of specializations of
                                           this function built yet *)
}
