(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* ************************************************************************* *)
(** {2 Handling groups of parameters} *)
(* ************************************************************************* *)

val get_selection_context: ?is_set:bool -> unit -> State_selection.t
(** Selection of all the parameters that may have an impact on some
    analysis. *)

val get_selection: ?is_set:bool -> unit -> State_selection.t
(** Selection of all the settable parameters.
    [is_set] is [true] by default (for backward compatibility): in such a
    case, for each option, the extra internal state indicating whether it is
    set also belongs to the selection.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val get_reset_selection: ?is_set:bool -> unit -> State_selection.t
(** Selection of resettable parameters in case of copy with a visitor.
    Not for casual user.
    @since Neon-20140301 *)

(**/**)

(* ************************************************************************* *)
(** {2 Generic implementation of command line option} *)
(* ************************************************************************* *)

module Make
    (_: sig val shortname: string end)
    (X:sig
       include Datatype.S
       val default: unit -> t
       val option_name: string
       val functor_name: string
     end):
sig
  include Parameter_sig.S_no_parameter with type t = X.t
  module Is_set: State_builder.S
  val group: Cmdline.Group.t
  val stage: Cmdline.stage
  val is_visible: bool
  val is_safe: bool
  val is_dynamic: bool
  val register_dynamic:
    string -> 'arg Type.t -> 'ret Type.t -> ('arg -> 'ret) -> 'arg -> 'ret
end

(**/**)

val collections: State.Set.t ref
