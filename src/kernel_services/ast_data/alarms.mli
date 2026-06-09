(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Alarms Database. *)

open Cil_types

(** Only signed overflows and pointer downcasts are really RTEs.
    The other kinds may be meaningful nevertheless. *)
type overflow_kind =
    Signed | Unsigned | Signed_downcast | Unsigned_downcast | Pointer_downcast

type access_kind = For_reading | For_writing
type bound_kind = Lower_bound | Upper_bound

type alarm =
  | Division_by_zero of exp
  | Memory_access of lval * access_kind
  | Index_out_of_bound of exp * exp option
  (** [Index_out_of_bound(index, opt)]
      - [opt = None] -> lower bound is zero; Some up = upper bound *)
  | Invalid_pointer of exp
  | Unaligned_pointer of exp * typ
  (** [Unaligned_pointer(p, t)] The alignment of the pointer [p] is not
      suitable for an access with type [t] *)
  | Invalid_shift of exp * int option (** strict upper bound, if any *)
  | Pointer_comparison of exp option * exp
  (** First parameter is [None] when implicit comparison to NULL pointer *)
  | Differing_blocks of exp * exp
  (** The two expressions (which evaluate to
      pointers) must point to the same allocated block *)
  | Overflow of overflow_kind * exp * Z.t * bound_kind
  (** Integer parameters is the bound *)
  | Float_to_int of exp * Z.t * bound_kind
  (** Integer parameter is the bound for the integer type. The actual alarm
      is [exp < bound+1] or [bound-1 < exp]. *)
  | Not_separated of lval * lval
  (** the two lvalues must be separated *)
  | Overlap of lval * lval
  (** overlapping read/write: the two lvalues must be
      separated or equal *)
  | Uninitialized of lval
  | Dangling of lval
  | Is_nan_or_infinite of exp * fkind
  | Is_nan of exp * fkind
  | Function_pointer of exp * exp list option
  (** the type of the pointer is compatible with the type of the pointed
      function (first argument). The second argument is the list of the
      arguments of the call. *)
  | Invalid_bool of lval (** Trap representation of a _Bool variable. *)

include Datatype.S_with_collections with type t = alarm

val self: State.t

val register:
  Emitter.t -> ?kf:kernel_function -> kinstr -> ?loc:location ->
  ?status:Property_status.emitted_status -> alarm ->
  code_annotation * bool
(** Register the given alarm on the given statement. By default, no status is
    emitted. [kf] must be given only if the [kinstr] is a statement, and
    must be the function enclosing this statement.
    @return true if the given alarm has never been emitted before on the
    same kinstr (without taking into consideration the status or
    the emitter).
*)

val to_annot: kinstr -> ?loc:location -> alarm -> code_annotation * bool
(** Conversion of an alarm to a [code_annotation], without any registration.
    The returned boolean indicates that the alarm has not been registered
    in the kernel yet. *)

val iter:
  (Emitter.t -> kernel_function -> stmt -> rank:int -> alarm -> code_annotation
   -> unit)
  -> unit
(** Iterator over all alarms and the associated annotations at some program
    point.
    @since Fluorine-20130401 *)

val fold:
  (Emitter.t -> kernel_function -> stmt -> rank:int -> alarm -> code_annotation
   -> 'a
   -> 'a)
  -> 'a
  -> 'a
(** Folder over all alarms and the associated annotations at some program
    point.
    @since Fluorine-20130401 *)

val to_seq:
  unit ->
  (Emitter.t * kernel_function * stmt * int * alarm * code_annotation) Seq.t
(** Returns the sequence of all alarms and the associated annotations at some
    program point
    @since 26.0-Iron *)

val find: code_annotation -> alarm option
(** @return the alarm corresponding to the given assertion, if any.
    @since Fluorine-20130401 *)

val remove: ?filter:(alarm -> bool) -> ?kinstr:kinstr -> Emitter.t -> unit
(** Remove the alarms and the associated annotations emitted by the given
    emitter. If [kinstr] is specified, remove only the ones associated with this
    kinstr. If [filter] is specified, remove only the alarms [a] such that
    [filter a] is [true].
    @since Fluorine-20130401 *)

val create_predicate: ?loc:location -> t -> predicate
(** Generate the predicate corresponding to a given alarm.
    @since Fluorine-20130401 *)

val get_name: t -> string
(** Short name of the alarm, used to prefix the assertion in the AST. *)

val get_short_name: t -> string
(** Even shorter name. Similar alarms (e.g. signed overflow vs. unsigned
    overflow) are aggregated. *)

val get_description: t -> string
(** Long description of the alarm, explaining the UB it guards against. *)
