(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Extend the environment with statements which allocate/deallocate memory
    blocks. *)

open Cil_types
open Cil_datatype

val store: Env.t -> kernel_function -> varinfo list -> Env.t
(** For each variable of the given list, if necessary according to the mtracking
    analysis, add a call to [__e_acsl_store_block] in the given environment. *)

val duplicate_store: Env.t -> kernel_function -> Varinfo.Set.t -> Env.t
(** Same as [store], with a call to [__e_acsl_duplicate_store_block]. *)

val delete_from_list: Env.t -> kernel_function -> varinfo list -> Env.t
(** Same as [store], with a call to [__e_acsl_delete_block]. *)

val delete_from_set: Env.t -> kernel_function -> Varinfo.Set.t -> Env.t
(** Same as [delete_from_list] with a set of variables instead of a list. *)
