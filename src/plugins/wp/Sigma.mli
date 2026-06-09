(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module F = Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Memory Environments                                                --- *)
(* -------------------------------------------------------------------------- *)

(** Memory Environments.

    The concrete memory is partionned into a vector of abstract data.
    Each component of the partition is called a {i memory chunk} and
    holds an abstract representation of some part of the memory.

    A sigma assigns a logical variable to each {i memory chunk}, assigned
    lazily, that represents the contents of this slice of memory at a given
    program point.

    Remark: memory chunks are not required to be independent from each other,
    provided the memory model implementation is consistent with the chosen
    representation. Conversely, a given object might be represented by
    several memory chunks.
*)

(** Memory chunk types.

    The type of chunks is extensible.
    Each memory model extends this type with of its {i own} memory chunks
    by functor {!Make}. *)
type chunk

(** Memory chunks at a given program point.

    This is a collection of variables indexed by chunks.
    New chunks are generated from the context pool of {!Lang.freshvar}. *)
type sigma

(** {1 Chunks API} *)

module type ChunkType =
sig

  type t

  val self : string
  (** Chunk type name for datatype registration. *)

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit

  val tau_of_chunk : t -> F.tau
  (** The type of data hold in a chunk. *)

  val basename_of_chunk : t -> string
  (** Used when generating fresh variables for a chunk. *)

  val is_init : t -> bool
  (** Whether the chunk tracks memory initialization (used for filtering). *)

  val is_primary : t -> bool
  (** Used to sort memory chunk parameters in compiled ACSL symbols.

      Non-primary chunk parameters come first, followed by primary ones,
      followed by logical parameters.

      Typical primary chunks are memory chunks with a single location,
      like hoare of ref variables in MemVar or singleton regions in
      MemRegion. *)

  val is_framed : t -> bool
  (** Whether the chunk is local to a function call.

      Means the chunk is separated from another call side-effects.
      If [true], entails that a function assigning everything can not modify
      the chunk. Only used for optimisation, it would be safe to always
      return [false]. *)
end

(** Uniform access to chunks features. *)
module Chunk : ChunkType with type t = chunk

(** Uniform Sets and Maps of chunks. *)
module Heap : Qed.Collection.S with type t = chunk
module Domain = Heap.Set

(** Memory footprint, ie. set of memory model chunk types. *)
type domain = Domain.t

(** Internal representation of chunks (chunk-kind).
    This type can only be extended via functor {!Make}. *)
type ckind = private ..

(** Access to internal chunk representation. *)
val ckind : chunk -> ckind

(** Chunk Extension functor.

    This functor promote an individual chunk type of
    some memory model into an uniform chunk.
*)
module Make(C : ChunkType) :
sig
  (** Chunk Extension. *)
  type ckind += Mu of C.t

  val chunk : C.t -> chunk
  (** Individual promotion of a model chunk to a uniform chunk. *)

  val singleton : C.t -> domain
  (** Promoted [Domain.singleton]. *)

  val mem : sigma -> C.t -> bool
  (** Specialized [Sigma.mem], equivalent to [Sigma.mem s (chunk c)]. *)

  val get : sigma -> C.t -> F.var
  (** Specialized [Sigma.get], equivalent to [Sigma.get s (chunk c)]. *)

  val value : sigma -> C.t -> F.term
  (** Specialized [Sigma.value], equivalent to [Sigma.value s (chunk c)]. *)

end

(** {1 Sigma API} *)

val create : unit -> sigma
(** Initially empty environment. *)

val pretty : Format.formatter -> sigma -> unit
(** For debugging purpose *)

val mem : sigma -> chunk -> bool
(** Whether a chunk has been assigned. *)

val get : sigma -> chunk -> F.var
(** Lazily get the variable for a chunk. *)

val value : sigma -> chunk -> F.term
(** Same as [Lang.F.e_var] of [get]. *)

val copy : sigma -> sigma
(** Duplicate the environment. Fresh chunks in the copy
    are {i not} duplicated into the source environment. *)

val join : sigma -> sigma -> Passive.t
(** Make two environment pairwise equal {i via} the passive form.

    Missing chunks in one environment are added with the corresponding
    variable of the other environment. When both environments don't agree
    on a chunk, their variables are added to the passive form. *)

val assigned : pre:sigma -> post:sigma -> domain -> F.pred Bag.t
(** Make chunks equal outside of some domain.

    This is similar to [join], but outside the given footprint of an
    assigns clause. Although, the function returns the equality
    predicates instead of a passive form.

    Like in [join], missing chunks are reported from one side to the
    other one, and common chunks are added to the equality bag. *)

val choose : sigma -> sigma -> sigma
(** Make the union of each sigma, choosing the minimal variable
    in case of conflict.
    Both initial environments are kept unchanged. *)

val merge : sigma -> sigma -> sigma * Passive.t * Passive.t
(** Make the union of each sigma, choosing a {i new} variable for
    each conflict, and returns the corresponding joins.
    Both initial environments are kept unchanged. *)

val merge_list : sigma list -> sigma * Passive.t list
(** Same than {!merge} but for a list of sigmas. Much more efficient
    than folding merge step by step. *)

val iter : (chunk -> F.var -> unit) -> sigma -> unit
(** Iterates over the chunks and associated variables already
    accessed so far in the environment. *)

val iter2 :
  (chunk -> F.var option -> F.var option -> unit) ->
  sigma -> sigma -> unit
(** Same as [iter] for both environments. *)

val havoc_chunk : sigma -> chunk -> sigma
(** Generate a new fresh variable for the given chunk. *)

val havoc : sigma -> domain -> sigma
(** All the chunks in the provided footprint are generated and made fresh.

    Existing chunk variables {i outside} the footprint are copied into the new
    environment. The original environment itself is kept unchanged. More
    efficient than iterating [havoc_chunk] over the footprint.
*)

val havoc_any : call:bool -> sigma -> sigma
(** All the chunks are made fresh. As an optimisation,
    when [~call:true] is set, only non-local chunks are made fresh.
    Local chunks are those for which [Chunk.is_frame] returns [true]. *)

val remove_chunks : sigma -> domain -> sigma
(** Return a copy of the environment where chunks in the footprint
    have been removed. Keep the original environment unchanged. *)

val is_init : chunk -> bool
(** Shortcut to [Chunk.is_init] *)

val is_framed : chunk -> bool
(** Shortcut to [Chunk.is_framed] *)

val domain : sigma -> domain
(** Footprint of a memory environment.
    That is, the set of accessed chunks so far in the environment. *)

val union : domain -> domain -> domain
(** Same as [Domain.union] *)

val empty : domain
(** Same as [Domain.empty] *)

val writes : pre:sigma -> post:sigma -> domain
(** [writes s] indicates which chunks are new in [s.post] compared
    to [s.pre]. *)

type state = chunk F.Tmap.t
val state : sigma -> state
val apply : (F.term -> F.term) -> state -> state
