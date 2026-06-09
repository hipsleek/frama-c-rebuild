(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {2 Sanitizer}

    Keeps only alpha-numerical characters.
    Separator ['_'] is allowed, but leading, trailing and consecutive
    separators are removed.
*)

type buffer

val create : ?truncate:bool -> int -> buffer
val clear : buffer -> unit

val add_sep : buffer -> unit (** Adds ['_'] character *)

val add_char : buffer -> char -> unit
val add_string : buffer -> string -> unit
val add_list : buffer -> string list -> unit (** Separated with ['_'] *)

val contents : buffer -> string
