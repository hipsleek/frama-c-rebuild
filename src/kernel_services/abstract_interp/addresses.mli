(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Represent a set of addresses by associating bases with offsets. *)

(** Association between bases and offsets in byte.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @before 33.0-Arsenic was Locations.Location_Bytes *)
module Bytes : sig
  (* TODOBY: write an mli for MapLattice, and name the result. Use it there,
     and simplify *)

  module M : sig
    type key = Base.t

    (** Mapping from bases to bytes-expressed offsets *)
    type t
    val iter : (Base.t -> Ival.t -> unit) -> t -> unit
    val find :  key -> t -> Ival.t
    val fold : (Base.t -> Ival.t -> 'a -> 'a) -> t -> 'a -> 'a
    val shape: t -> Ival.t Hptmap.Shape(Base.Base).t
  end

  type t = private
    | Top of Base.SetLattice.t * Origin.t
    (** Garbled mix of the addresses in the set *)
    | Map of M.t (** Precise set of addresses+offsets *)

  (** Address sets have a lattice structure, including standard operations
      such as [join], [narrow], etc. *)
  include Lattice_type.AI_Lattice_with_cardinal_one with type t := t

  type widen_hint = Ival.widen_hint

  val widen : ?size:Z.t -> ?hint:widen_hint -> t -> t -> t

  include Datatype.S_with_collections with type t := t

  val singleton_zero : t
  (** the set containing only the value for to the C expression [0] *)

  val singleton_one : t
  (** the set containing only the value [1] *)

  val zero_or_one : t

  val is_zero : t -> bool
  val is_bottom : t -> bool

  val top_int : t
  val top_float : t
  val top_single_precision_float : t

  val inject : Base.t -> Ival.t -> t
  val inject_ival : Ival.t -> t
  val inject_float : Fval.F.t -> t

  val add : Base.t -> Ival.t ->  t ->  t
  (** [add b i addr] binds [b] to [i] in [addr] when [i] is not {!Ival.bottom},
      and returns {!bottom} otherwise. *)

  val replace_base: Base.substitution -> t -> bool * t
  (** [replace_base subst addr] changes addresses [addr] by substituting the
      pointed bases according to [subst]. If [substitution] conflates different
      bases, the offsets bound to these bases are joined. *)

  val overlaps: partial:bool -> size:Z.t -> t -> t -> bool
  (** Is there a possibly non-empty intersection between two given locations
      represented by the starting addresses and a given size?
      If [partial] is true, returns true if the two locations may be overlapping
      without being equal. If [partial] is false, also returns true if the two
      locations may be equal. Returns false when the two locations cannot be
      overlapping. *)

  val diff : t -> t -> t
  (** Over-approximation of difference. [arg2] needs to be exact or an
      under_approximation. *)

  val diff_if_one : t -> t -> t
  (** Over-approximation of difference. [arg2] can be an
      over-approximation. *)

  val shift : Ival.t -> t -> t
  val shift_under : Ival.t -> t -> t
  (** Over- and under-approximation of shifting the value by the given Ival. *)

  val sub_pointwise: ?factor:Z.t -> t -> t -> Ival.t
  (** Subtracts the offsets of two address sets [addr1] and [addr2].
      Returns the pointwise subtraction of their offsets
      [off1 - factor * off2]. [factor] defaults to [1]. *)

  val sub_pointer: t -> t -> t
  (** Subtracts the offsets of two address sets. Same as [sub_pointwise factor:1],
      except that garbled mixes from operands are propagated into the result. *)

  val topify: Origin.kind -> t -> t
  (** [topify kind v] converts [v] to a garbled mix of the addresses pointed to
      by [v], with origin [kind]. Returns [v] unchanged if it is bottom,
      the singleton zero or already a garbled mix. *)

  val topify_with_origin: Origin.t -> t -> t
  (** Same as [topify] above with the given origin. *)

  val inject_top_origin : Origin.t -> Base.Hptset.t -> t
  (** [inject_top_origin origin bases] creates a garbled mix of bases [bases]
      with origin [origin]. *)

  val top_with_origin: Origin.t -> t
  (** Completely imprecise value. Use only as last resort. *)

  (* {2 Iterators} *)

  val fold_bases : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold on all the bases of the address sets, including [Top bases].
      @raise Abstract_interp.Error_Top in the case [Top Top]. *)

  val fold_i : (Base.t -> Ival.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold with offsets.
      @raise Abstract_interp.Error_Top in the cases [Top Top], [Top bases]. *)

  val fold_topset_ok: (Base.t -> Ival.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold with offsets, including in the case [Top bases]. In this case,
      [Ival.top] is supplied to the iterator.
      @raise Abstract_interp.Error_Top in the case [Top Top]. *)

  val fold_enum : (t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_enum f addr acc] enumerates addresses in [addr], and passes
      them to [f]. Make sure to call {!cardinal_less_than} before calling
      this function, as all possible combinations of bases/offsets are
      presented to [f]. Raises {!Abstract_interp.Error_Top} if [addr] is
      [Top _] or if one offset cannot be enumerated. *)

  val to_seq_i : t -> (Base.t * Ival.t) Seq.t
  (** Builds a sequence of all bases (with their offsets) of the address set.
      @raise Abstract_interp.Error_Top in the cases [Top _]. *)

  val cached_fold:
    cache:Hptmap_sig.cache_type ->
    f:(Base.t -> Ival.t -> 'a) ->
    joiner:('a -> 'a -> 'a) -> empty:'a -> t -> 'a
  (** Cached version of [fold_i], for advanced users *)

  val for_all: (Base.t -> Ival.t -> bool) -> t -> bool
  val exists: (Base.t -> Ival.t -> bool) -> t -> bool

  val filter_base : (Base.t -> bool) -> t -> t


  (** {2 Number of addresses} *)

  val cardinal_zero_or_one : t -> bool
  val cardinal_less_than : t -> int -> int
  (** [cardinal_less_than v card] returns the cardinal of [v] if it is less
      than [card], or raises [Not_less_than]. *)

  val cardinal: t -> Z.t option (** None if the cardinal is unbounded *)

  val find_lonely_key : t -> Base.t * Ival.t
  (** if there is only one base [b] in the address set, then returns the
      pair [b,o] where [o] are the offsets associated to [b].
      @raise Not_found otherwise. *)

  val find_lonely_binding : t -> Base.t * Ival.t
  (** if there is only one binding [b -> o] in the address set (that is, only
      one base [b] with [cardinal_zero_or_one o]), returns the pair [b,o].
      @raise Not_found otherwise *)


  (** {2 Destructuring} *)
  val find: Base.t -> t -> Ival.t
  val find_or_bottom : Base.t -> M.t -> Ival.t
  val split : Base.t -> t -> Ival.t * t

  val get_bases : t -> Base.SetLattice.t
  (** Returns the bases the addresses may point to. Never fails, but
      may return [Base.SetLattice.Top]. *)


  (** {2 Local variables inside locations} *)

  val contains_addresses_of_locals : (M.key -> bool) -> t -> bool
  (** [contains_addresses_of_locals is_local addr] returns [true]
      if [addr] contains the address of a variable for which
      [is_local] returns [true] *)

  val remove_escaping_locals : (M.key -> bool) -> t -> bool * t
  (**  [remove_escaping_locals is_local addr] removes from [addr] the information
       associated with bases for which [is_local] returns [true]. The
       returned boolean indicates that [addr] contained some locals. *)

  val contains_addresses_of_any_locals : t -> bool
  (** [contains_addresses_of_any_locals addr] returns [true] iff [addr] contains
      the address of a local variable or of a formal variable. *)

  (** {2 Misc} *)

  (** [is_relationable addr] returns [true] iff [addr] represents a single
      memory address. *)
  val is_relationable: t -> bool

  val may_reach : Base.t -> t -> bool
  (** [may_reach base addr] is true if [base] might be accessed from [addr]. *)

  (**/**)
  val pretty_debug: t Pretty_utils.formatter
  val clear_caches: unit -> unit
end

(** Association between bases and offsets in bits.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @before 33.0-Arsenic was Locations.Location_Bits *)
module Bits : sig
  include module type of Bytes

  (** {2 Conversion functions} *)

  (** [of_bytes a] returns the address set [a] with the offset converted from
      bytes to bits. The result is exact.
      @before 33.0-Arsenic was Locations.loc_bytes_to_loc_bits *)
  val of_bytes : Bytes.t -> t

  (** [to_bytes a] returns the address set [a] with the offset converted from
      bits to bytes. The result is an over-approximation.
      @before 33.0-Arsenic was Locations.loc_bits_to_loc_bytes *)
  val to_bytes : t -> Bytes.t

  (** [to_bytes_under a] returns the address set [a] with the offsets
      converted from bits to bytes. The result is an under-approximation.
      @before 33.0-Arsenic was Locations.loc_bits_to_loc_bytes_under *)
  val to_bytes_under : t -> Bytes.t
end
