(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Register the plugin in the Frama-C kernel. *)

open Cil_types

val self: State.t

val get_last_result:
  unit -> ((kernel_function option * kinstr * lval) list * varinfo) option

val get: (varinfo -> (kernel_function option * kinstr * lval) list)
(** Return the occurrences of the given varinfo.
    An occurrence [ki, lv] is a left-value [lv] which uses the location of
    [vi] at the position [ki]. *)

val print_all: (unit -> unit)
(** Print all the occurrence of each variable declarations. *)

type access_type = Read | Write | Both

val classify_accesses: kernel_function option * kinstr *lval -> access_type
