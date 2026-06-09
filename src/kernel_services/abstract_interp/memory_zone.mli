(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Association between bases and ranges of bits.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

type map_t

type t = private Top of Base.SetLattice.t * Origin.t | Map of map_t

include Datatype.S_with_collections with type t := t
val pretty_debug: t Pretty_utils.formatter

include Lattice_type.Bottom_Bounded_Join_Semi_Lattice with type t := t
include Lattice_type.With_Top with type t := t
include Lattice_type.With_Narrow with type t := t
include Lattice_type.With_Under_Approximation with type t := t
include Lattice_type.With_Diff with type t := t

val is_bottom: t -> bool
val is_top: t -> bool
val inject : Base.t -> Int_Intervals.t -> t
val add : Base.t -> Int_Intervals.t -> t -> t

val find_lonely_key : t -> Base.t * Int_Intervals.t
val find_or_bottom : Base.t -> map_t -> Int_Intervals.t
val find: Base.t -> t -> Int_Intervals.t

val mem_base : Base.t -> t -> bool
(** [mem_base b m] returns [true] if [b] is associated to something
    or topified in [t], and [false] otherwise.

    @since Carbon-20101201 *)

val get_bases : t -> Base.SetLattice.t
(** Returns the bases contained by the given zone. Never fails, but
    may return [Base.SetLattice.Top]. *)

val of_bases : Base.Hptset.t -> t
(** Returns the memory zone of a set of bases. *)

val intersects : t -> t -> bool

(** Assuming that [z1] and [z2] only contain valid bases,
    [valid_intersects z1 z2] returns true iff [z1] and [z2] have a valid
    intersection. *)
val valid_intersects : t -> t -> bool

(** {2 Folding} *)

val filter_base : (Base.t -> bool) -> t -> t
(** [filter_base] can't raise [Abstract_interp.Error_Top] since it
    filtersbases of [Top bases]. Note: the filter may give an
    over-approximation (in the case [Top Top]). *)

val fold_bases : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_bases] folds also bases of [Top bases].
    @raise Abstract_interp.Error_Top in the case [Top Top]. *)

val fold_i : (Base.t -> Int_Intervals.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_i f l acc] folds [l] by base.
    @raise Abstract_interp.Error_Top in the cases [Top Top], [Top bases]. *)

val fold_topset_ok : (Base.t -> Int_Intervals.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_i f l acc] folds [l] by base.
    @raise Abstract_interp.Error_Top in the case [Top Top]. *)

val cached_fold :
  cache:Hptmap_sig.cache_type ->
  f:(Base.t -> Int_Intervals.t -> 'b) ->
  joiner:('b -> 'b -> 'b) -> empty:'b -> t -> 'b

val fold2_join_heterogeneous:
  cache:Hptmap_sig.cache_type ->
  empty_left:('a Hptmap.Shape(Base.Base).t -> 'b) ->
  empty_right:(t -> 'b) ->
  both:(Base.t -> Int_Intervals.t -> 'a -> 'b) ->
  join:('b -> 'b -> 'b) ->
  empty:'b ->
  t -> 'a Hptmap.Shape(Base.Base).t ->
  'b


(** {2 Misc} *)
val shape: map_t -> Int_Intervals.t Hptmap.Shape(Base.Base).t

(**/**)
val clear_caches: unit -> unit
