(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Builds an environment to resolve names of globals and functions which
   can then be used, even if Frama-C global tables are not filled yet. *)

type t

val from_file : Cil_types.file -> t

val find_global : t -> string -> Cil_types.varinfo
val find_global_init: t -> string -> Cil_types.init_or_str option
val find_function : t -> string ->  Cil_types.varinfo
val find_typedef : t -> string ->  Cil_types.typeinfo
val find_struct : t -> string ->  Cil_types.compinfo
val find_union : t -> string ->  Cil_types.compinfo
val find_enum : t -> string ->  Cil_types.enuminfo
val find_type : t -> Logic_typing.type_namespace -> string -> Cil_types.typ

val mem_global : t -> string -> bool
val mem_function : t -> string -> bool
