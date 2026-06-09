(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  INSA (Institut National des Sciences Appliquees)                          *)
(*                                                                            *)
(******************************************************************************)

(** visitors performing the instrumentation *)

module Aux_funcs: sig
  (** the various kinds of auxiliary functions. *)
  type kind =
    | Not_aux_func (** original C function. *)
    | Aux of Cil_types.kernel_function
    (** Checks whether we are in the corresponding behavior
        of the function. *)
    | Pre of Cil_types.kernel_function
    (** [Pre_func f] denotes a function updating the automaton
        when [f] is called. *)
    | Post of Cil_types.kernel_function
    (** [Post_func f] denotes a function updating the automaton
        when returning from [f]. *)

  val iter: (Cil_types.varinfo -> kind -> unit) -> unit
  (** [iter f] calls [f] on all functions registered so far by
      {!add_sync_with_buch}
  *)

end

(** generate prototypes for auxiliary functions. *)
val add_sync_with_buch: Cil_types.file -> unit

(**
   [add_pre_post_from_buch ast treatloop]
   provide contracts and/or bodies for auxiliary function
   (once they have been generated). If [treatloop] is [true],
   loop invariants are also generated.
*)
val add_pre_post_from_buch: Cil_types.file -> bool -> unit
