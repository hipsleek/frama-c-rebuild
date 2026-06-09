(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Extensible Arrays *)
(* -------------------------------------------------------------------------- *)

type 'a t

val create : unit -> 'a t

val length : 'a t -> int
val size : 'a t -> int (** Same as [length] *)

val get : 'a t -> int -> 'a
(** Raise [Not_found] if out-of-bounds. *)

val set : 'a t -> int -> 'a -> unit
(** Raise [Not_found] if out-of-bounds. *)

val add : 'a t -> 'a -> unit
(** Element will be added at index [size]. After addition, it is at index [size-1]. *)

val addi : 'a t -> 'a -> int
(** Return index of added (last) element. *)

val clear : 'a t -> unit
(** Do not modify actual capacity. *)

val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
(** Result is shrunk. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** Result is shrunk. *)

val find : 'a t -> ?default:'a -> ?exn:exn -> int -> 'a
(** Default exception is [Not_found].
    If a [default] value is provided, no exception is raised. *)

val update : 'a t -> ?default:'a -> int -> 'a -> unit
(** Set value at index.
    If the updated index is greater of equal to the vector size,
    empty cells are inserted with the default value.
    @raise Invalid_argument if the index is negative or when it exceeds the
    the vector size but the default value is not provided. *)

val to_array : 'a t -> 'a array
(** Makes a copy. *)

val of_array : 'a array -> 'a t
(** Makes a copy. *)

(** Low-level interface. Internal capacity. *)
val capacity : 'a t -> int

(** Low-level interface. Sets internal capacity. Extra elements are removed. *)
val resize : 'a t -> int -> unit

(** Low-level interface. Sets capacity to content. *)
val shrink : 'a t -> unit
