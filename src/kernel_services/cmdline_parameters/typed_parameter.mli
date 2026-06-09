(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Parameter settable through a command line option.
    This is a low level API, internally used by the kernel. As a plug-in
    developer, you certainly prefer to use the API of {!Plugin} instead.
    @since Nitrogen-20111001
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
*)

(** generic accessor type *)
type ('a, 'b) gen_accessor =
  { get: unit -> 'a;
    set: 'a -> unit;
    add_set_hook: ('b -> 'b -> unit) -> unit;
    add_update_hook: ('b -> 'b -> unit) -> unit }

type 'a accessor = ('a, 'a) gen_accessor

type typed_accessor =
  | Bool of bool accessor * string option (** the negative option, if any *)
  | Int of int accessor * (unit -> int * int) (** getting range *)
  | Float of float accessor * (unit -> float * float) (** getting range *)
  | String of string accessor * (unit -> string list) (** possible values *)

type parameter = private
  { name: string; (** Name of the option corresponding to the parameter.
                      It is exactly the state name of the option (see
                      {!State.get_name}). *)
    help: string; (** Help message *)
    accessor: typed_accessor; (** How to get and set the value of the
                                  parameter *)
    visible: bool; (** Is visible to the user, e.g. in the command-line help *)
    safe: bool; (** Can be used in sandbox mode *)
    reconfigurable: bool; (** Can be reconfigured, e.g. in the GUI *)
    is_set: unit -> bool (** Is this option really set? *) }

include Datatype.S_with_collections with type t = parameter

val get: string -> t
(** Get the parameter from the option name. *)

val get_value: t -> string
(** Get the current value of the parameter, as a string. *)

(**/**)
(** Not for casual users. Use API of {!Plugin} instead. *)
val create:
  name:string ->
  help:string ->
  accessor:typed_accessor ->
  visible:bool ->
  safe:bool ->
  reconfigurable:bool ->
  is_set: (unit -> bool) ->
  t
(**/**)
