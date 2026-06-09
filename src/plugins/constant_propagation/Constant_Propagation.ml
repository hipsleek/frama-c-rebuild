(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: Constant_Propagation.mli,v 1.6 2008-04-01 09:25:20 uid568 Exp $ *)

(** Constant propagation analysis. *)

module Api : sig
  val get : Cil_datatype.Fundec.Set.t -> cast_intro:bool -> Project.t
  (** Propagate constant into the functions given by name.
      note: the propagation is performed into all functions when the set is
      empty; and casts can be introduced when [cast_intro] is true. *)

  val compute: unit -> unit
  (** Propagate constant into the functions given by the parameters (in the
      same way that {!get}. Then pretty print the resulting program.
      @since Beryllium-20090901 *)

  val self: State.t
  (** Internal state of the constant propagation plugin. *)

end = Api
