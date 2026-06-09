(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type call_site = (Cil_types.kernel_function * Cil_types.kinstr)
type t = call_site list

include Datatype.S_with_collections with type t := t

(* The callstacks manipulated here have the following invariant:
   - the callstack is never an empty list
   - the last item of the list has always a Kglobal
   - all elements of the list except the last have a Kstmt *)

val init : Cil_types.kernel_function -> t
val pop : t -> (Cil_types.kernel_function * Cil_types.stmt * t) option
val pop_downto : Cil_types.kernel_function -> t -> t
val top_kf : t -> Cil_types.kernel_function
val push : Cil_types.kernel_function * Cil_types.stmt -> t -> t

(* Dive use partial callstack where the first call in the callstack are
   abstracted away. Thus, Dive callstack are prefixes of complete callstacks. *)

(* [is_prefix sub full]  returns true whenever [sub] is a prefix of [full] *)
val is_prefix : t -> t -> bool

(* [truncate_to_sub full sub] removes [full] tail until [sub] becomes a suffix.
   Returns None if [sub] is not included in [full] *)
val truncate_to_sub : t -> t -> t option
