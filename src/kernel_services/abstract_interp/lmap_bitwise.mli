(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Functors making map indexed by zone.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

exception Bitwise_cannot_copy

module type Location_map_bitwise = sig

  type v

  type map

  type lmap = Top | Map of map | Bottom

  include Datatype.S with type t = lmap
  include Lattice_type.Bottom_Bounded_Join_Semi_Lattice with type t := t
  include Lattice_type.With_Top with type t := t

  module LOffset :
    Offsetmap_bitwise_sig.S
    with type v = v
     and type intervals = Int_Intervals.t

  val is_empty : t -> bool
  val is_bottom : t -> bool
  val empty : t
  val empty_map: map

  val pretty_generic_printer:
    ?pretty_v: v Pretty_utils.formatter ->
    ?skip_v: (v -> bool) ->
    sep:string ->
    unit ->
    t Pretty_utils.formatter

  val pretty_debug: t Pretty_utils.formatter

  val add_binding : exact:bool -> t -> Memory_zone.t -> v -> t
  val add_binding_loc: exact:bool -> t -> Locations.t -> v -> t
  val add_base: Base.t -> LOffset.t -> t -> t
  val remove_base: Base.t -> t -> t

  val find : t -> Memory_zone.t -> v Lattice_bounds.or_bottom

  val filter_base : (Base.t -> bool) -> t -> t


  (** {2 Iterators} *)

  val map: (v -> v) -> t -> t

  (** The following fold_* functions, as well as {!map2} take arguments
      of type [map] to force their user to handle the cases Top and Bottom
      explicitly. *)
  val fold: (Memory_zone.t -> v -> 'a -> 'a) -> map -> 'a -> 'a
  (** [fold f m] folds a function [f] on the bindings in [m]. Contiguous
      bits with the same value are merged into a single zone. Different bases
      are presented in different zones. *)

  val fold_base : (Base.t -> LOffset.t -> 'a -> 'a) -> map -> 'a -> 'a

  val fold_fuse_same : (Memory_zone.t -> v -> 'a -> 'a) -> map -> 'a -> 'a
  (** Same behavior as [fold], except if two non-contiguous ranges [r1] and
      [r2] of a given base are mapped to the same value.
      [fold] will call its argument [f] on each range successively
      (hence, in our example, on [r1] and [r2] separately).
      Conversely, [fold_fuse_same] will call [f] directly on [r1 U r2],
      U being the join on sets of intervals. *)

  val fold_join_zone:
    both:(Int_Intervals.t -> LOffset.t -> 'a) ->
    conv:(Base.t -> 'a -> 'b) ->
    empty_map:(Memory_zone.t -> 'b) ->
    join:('b -> 'b -> 'b) ->
    empty:'b ->
    Memory_zone.t -> map -> 'b
  (** [fold_join_zone ~both ~conv ~empty_map ~join ~empty z m] folds over the
      intervals present in [z]. When a base [b] is present in both [z] and [m],
      and bound respectively to [itvs] and [mb], [both itvs mb] is called.
      The results obtained for this base [b] are then converted using [conv].
      If a sub-zone [z'] is present in [z], but the corresponding bases are
      not bound in [m], [empty_map z'] is called. All the sub-results (of type)
      ['b] are joined using [join]. [empty] is used when an empty map or
      sub-zone is encountered. It must be a neutral element for [join].

      This function internally uses a cache, and {b must} be partially applied
      to its named arguments. (This explains the somewhat contrived interface,
      in particular the fact that [both] and [conv] are not fused.) *)

  val map2:
    cache:Hptmap_sig.cache_type -> symmetric:bool -> idempotent:bool ->
    empty_neutral: bool -> (LOffset.t -> LOffset.t -> LOffset.map2_decide) ->
    (v -> v -> v) -> map -> map -> map
  (** 'map'-like function between two interval maps, implemented as a
      simultaneous descent in both maps.
      [map2 ~cache ~symmetric ~idempotent ~empty_neutral decide_fast f m1 m2]
      computes the map containing [k |-> f v_1 v_2] for all the keys [k] present
      in either [m1] or [m2]. When a key is present, [v_i] is the corresponding
      value in the map. When it is missing in one of the maps, a default value is
      generated. (See argument [default] to functor {!Make_bitwise} below.)

      [symmetric], [idempotent], [empty_neutral] and [decide_fast] are present
      for optimisation purposes, to avoid visiting some trees. If [symmetric]
      holds, [f v1 v2 = f v2 v1] must also holds. If [idempotent] holds,
      [f v v = v] must also holds. Similarly, if [empty_neutral] holds,
      [f v default = f default v = v] must hold. [decide_fast] is called before
      visiting two subtrees, and can be used to stop the recursion early. See
      the documentation of {!Offsetmap_sig.map2_decide}.

      Depending on the value of [cache], the results of this function will be
      cached. *)

  (** {2 Misc} *)

  val shape: map -> LOffset.t Hptmap.Shape(Base.Base).t

  (** Clear the caches local to this module. Beware that they are not
      project-aware, and that you must call them at every project switch. *)
  val clear_caches: unit -> unit

end

module type With_default = sig
  include Lattice_type.Join_Semi_Lattice
  include Lattice_type.With_Top with type t := t
  val default: t
  val default_is_bottom : bool
end

module Make_bitwise(V : With_default) : Location_map_bitwise with type v = V.t
