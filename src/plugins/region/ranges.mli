(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val gcd : int -> int -> int
val (%.) : int -> int -> int (** gcd *)

type 'a range = { offset : int; length : int; data : 'a; }
type 'a t = private R of 'a range list (* sorted, no overlap *)

(** Prints [offset..last] formatted with [%04d] *)
val pp_range : Format.formatter -> 'a range -> unit

(** Prints [offset:length] formatted with [%04d] *)
val pp_offset : Format.formatter -> 'a range -> unit

val empty : 'a t
val singleton : 'a range -> 'a t
val range : ?offset:int -> ?length:int -> 'a -> 'a t
val merge : ('a range -> 'a range -> 'a) -> 'a t -> 'a t -> 'a t

val find : int -> 'a t -> 'a range

val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : ('a range -> 'b range) -> 'a t -> 'b t
val iter : ('a -> unit) -> 'a t -> unit
val iteri : ('a range -> unit) -> 'a t -> unit
val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val foldi : ('b -> 'a range -> 'b) -> 'b -> 'a t -> 'b
