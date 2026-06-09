(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  Jean-Christophe Filliatre                                             *)
(*  Modified by                                                           *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(*s This module implements {\em tries}. Given a map [M] over an
    arbitrary type [M.key], the following functor constructs a new map
    over type [M.key list]. *)

(** [S] is a subsignature of function result [Stdlib.Map.Make] *)
module type S = sig
  type key
  type +'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val remove : key -> 'a t -> 'a t
  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t ->  'b t -> 'c t
  val union :
    (key -> 'a -> 'a -> 'a option) -> 'a t ->  'a t -> 'a t
  val mem : key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val to_seq : 'a t -> (key * 'a) Seq.t
end

(** Builds a Map over [M.key list] from a map [M]. Note that the key lists are
    stored in reverse order and not reversed back each time a key list is given
    back to the caller, for optimization reasons. This applies to the parameter
    of [merge], [union], [iter], [mapi], [fold], [exists] and to the sequences
    produced by [to_seq]. *)
module Make(M : S) : sig
  include S with type key = M.key list

  (** Add a common prefix to all keys in the map *)
  val add_prefix: M.key -> 'a t -> 'a t

  (** Select the keys starting with the given prefix, removes this prefix
      and filters out keys without this prefix.
      @raise Not_found if no such prefix exists in the map. *)
  val select_prefix: M.key -> 'a t -> 'a t

  (** Builds a sequence of prefixes with the map that results from each prefix
      selection. *)
  val prefixes_seq: 'a t -> (M.key * 'a t) Seq.t
end
