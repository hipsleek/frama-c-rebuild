(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Name of concurrency primitives. Used for display to the user and
   identifying threads between analyses. *)

module Name : sig
  include Datatype.S_with_collections
  val of_cvalue : Cvalue.V.t -> t option
  val of_string : string -> t
  val to_string : t -> string
end
