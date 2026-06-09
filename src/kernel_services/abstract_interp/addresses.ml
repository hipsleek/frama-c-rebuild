(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Abstract_interp

module Hptmap_Info = struct
  let initial_values = [ [Base.null,Ival.zero];
                         [Base.null,Ival.one];
                         [Base.null,Ival.zero_or_one];
                         [Base.null,Ival.top];
                         [Base.null,Ival.top_float];
                         [Base.null,Ival.top_single_precision_float];
                         [Base.null,Ival.float_zeros];
                       ]

  let dependencies = [ Ast.self ]
end

(* Store the information that the location has at most cardinal 1, ignoring
   weak bases. The rationale is as follows: this compositional bool is used
   to improve the performance of slevel, to detect the parts of memory states
   that are "exact". Intuitively, locations that involve weak bases do not
   qualify. However, "exact" must be understood w.r.t. the [is_included]
   function: a value is "exact" if no other value than itself and bottom
   are included in it. Said otherwise, we do not consider the cardinality
   of the concretization, but instead the one of the Ocaml datastructure. *)
module Comp_exact = struct
  let empty = true (* corresponds to bottom *)

  let leaf _b v = Ival.cardinal_zero_or_one v
  (* on Ival, both forms of cardinal coincide *)

  let compose _ _ = false
  (* Keys cannot be bound to Bottom (see MapLattice). Hence, two subtrees have
     a t least cardinal two. *)
end


module Bytes = struct

  module M = struct
    include Hptmap.Make_with_compositional_bool
        (Base.Base) (Ival) (Comp_exact) (Hptmap_Info)
    let shape x = x
  end
  let () = Ast.add_monotonic_state M.self
  let clear_caches = M.clear_caches

  module MapLattice = struct
    include Map_lattice.Make_Map_Lattice (Base) (Ival) (M)
    include With_Cardinality (struct let is_summary = Base.is_weak end) (Ival)
  end

  module MapSetLattice = struct
    include Map_lattice.Make_MapSet_Lattice
        (Base.Base) (Base.SetLattice) (Ival) (MapLattice)
    include With_Cardinality (MapLattice)
  end

  include MapSetLattice
  (* Invariant :
     [Top (s, _) must always contain NULL, _and_ at least another base.
     Top ({Null}, _) is replaced by Top_int]. See inject_top_origin_internal
     below. *)

  let find_or_bottom = MapLattice.find_or_bottom
  let is_bottom = equal bottom

  let filter_base = filter_keys
  let fold_bases = fold_keys
  let fold_i f t acc = match t with
    | Top _ -> raise Error_Top
    | Map m -> MapLattice.fold f m acc
  let fold_topset_ok = fold
  let to_seq_i = function
    | Top _ -> raise Error_Top
    | Map m -> MapLattice.to_seq m
  let inject_ival i = inject Base.null i

  let inject_float f =
    inject_ival
      (Ival.inject_float
         (Fval.inject_singleton f))

  (** Check that those values correspond to {!Initial_Values} above. *)
  let singleton_zero = inject_ival Ival.zero
  let singleton_one = inject_ival Ival.one
  let zero_or_one = inject_ival Ival.zero_or_one
  let top_int = inject_ival Ival.top
  let top_float = inject_ival Ival.top_float
  let top_single_precision_float = inject_ival Ival.top_single_precision_float

  (* true iff [v] is exactly 0 *)
  let is_zero v = equal v singleton_zero

  (* [shift offset l] is the location [l] shifted by [offset] *)
  let shift offset l =
    if Ival.is_bottom offset then bottom
    else map (Ival.add_int offset) l

  (* [shift_under offset l] is the location [l] (an
     under-approximation) shifted by [offset] (another
     under-approximation); returns an underapproximation. *)
  let shift_under offset l =
    if Ival.is_bottom offset then bottom
    else map (Ival.add_int_under offset) l

  let sub_pointwise_map ?(factor=Z.one) m1 m2 =
    let factor = Z.neg factor in
    (* Subtract pointwise for all the bases that are present in both m1
       and m2. *)
    M.fold2_join_heterogeneous
      ~cache:Hptmap_sig.NoCache
      ~empty_left:(fun _ -> Ival.bottom)
      ~empty_right:(fun _ -> Ival.bottom)
      ~both:(fun _b i1 i2 -> Ival.add_int i1 (Ival.scale factor i2))
      ~join:Ival.join
      ~empty:Ival.bottom
      m1 m2

  let sub_pointwise ?factor l1 l2 =
    match l1, l2 with
    | Top _, Top _
    | Top (Base.SetLattice.Top, _), Map _
    | Map _, Top (Base.SetLattice.Top, _) -> Ival.top
    | Top (Base.SetLattice.Set s, _), Map m
    | Map m, Top (Base.SetLattice.Set s, _) ->
      let s' = Base.SetLattice.O.add Base.null s in
      if M.exists (fun base _ -> Base.SetLattice.O.mem base s') m then
        Ival.top
      else
        Ival.bottom
    | Map m1, Map m2 -> sub_pointwise_map ?factor m1 m2

  let sub_pointer l1 l2 =
    match l1, l2 with
    | Top (s1, o1), Top (s2, o2) ->
      if Base.SetLattice.intersects s1 s2
      then Top (Base.SetLattice.join s1 s2, Origin.join o1 o2)
      else bottom
    | Top (s, _) as t, Map m
    | Map m, (Top (s, _) as t) ->
      if Base.SetLattice.exists (fun b -> M.mem b m) s then t else bottom
    | Map m1, Map m2 ->
      let ival = sub_pointwise_map m1 m2 in
      inject_ival ival

  let cardinal_zero_or_one = function
    | Top _ -> false
    | Map m ->
      M.is_empty m ||
      M.on_singleton
        (fun b i -> not (Base.is_weak b) && Ival.cardinal_zero_or_one i) m

  let cardinal = function
    | Top _ -> None
    | Map m ->
      let aux_base b i card =
        if Base.is_weak b then None
        else
          match card, Ival.cardinal i with
          | None, _ | _, None -> None
          | Some c1, Some c2 -> Some (Z.add c1 c2)
      in
      M.fold aux_base m (Some Z.zero)

  let top_with_origin origin = Top (Base.SetLattice.top, origin)

  let inject_top_origin o b =
    if Base.Hptset.(equal b empty || equal b Base.null_set) then
      top_int
    else begin
      let bases = Base.SetLattice.inject Base.(Hptset.add null b) in
      Origin.register bases o;
      Top (bases, o)
    end

  (** some functions can reduce a garbled mix, make sure to normalize
      the result when only NULL remains *)
  let normalize_top m =
    match m with
    | Top (Base.SetLattice.Top, _) | Map _ -> m
    | Top (Base.SetLattice.Set s, o) -> inject_top_origin o s

  let narrow m1 m2 = normalize_top (narrow m1 m2)
  let meet m1 m2 = normalize_top (meet m1 m2)

  let is_top = function
    | Top _ -> true
    | Map _ -> false

  let topify_with_origin o v =
    if is_top v || is_zero v || is_bottom v then v
    else
      match get_keys v with
      | Base.SetLattice.Top -> top_with_origin o
      | Base.SetLattice.Set b -> inject_top_origin o b

  let topify kind = topify_with_origin (Origin.current kind)

  let get_bases = get_keys

  let is_relationable m =
    try
      let b,_ = find_lonely_binding m in
      match Base.validity b with
      | Base.Empty | Base.Known _ | Base.Unknown _ | Base.Invalid -> true
      | Base.Variable { Base.weak } -> not weak
    with Not_found -> false

  let may_reach base loc =
    if Base.is_null base then true
    else
      match loc with
      | Top (Base.SetLattice.Top, _) -> true
      | Top (Base.SetLattice.Set s,_) ->
        Base.Hptset.mem base s
      | Map m -> try
          ignore (M.find base m);
          true
        with Not_found -> false

  let contains_addresses_of_locals is_local l =
    match l with
    | Top (Base.SetLattice.Top,_) -> true
    | Top (Base.SetLattice.Set s, _) ->
      Base.SetLattice.O.exists is_local s
    | Map m ->
      M.exists (fun b _ -> is_local b) m

  let remove_escaping_locals is_local v =
    let non_local b = not (is_local b) in
    match v with
    | Top (Base.SetLattice.Top,_) -> true, v
    | Top (Base.SetLattice.Set garble, orig) ->
      let nonlocals = Base.Hptset.filter non_local garble in
      if Base.Hptset.equal garble nonlocals then
        false, v
      else
        true, inject_top_origin orig nonlocals
    | Map m ->
      let nonlocals = M.filter non_local m in
      if M.equal nonlocals m then
        false, v
      else
        true, Map nonlocals

  let contains_addresses_of_any_locals =
    let f base _offsets = Base.is_any_formal_or_local base in
    let cache = Hptmap_sig.PersistentCache "loc_top_any_locals" in
    let cached_f = cached_fold ~cache ~f ~joiner:(||) ~empty:false in
    fun loc ->
      try
        cached_f loc
      with Error_Top ->
        assert (match loc with
            | Top (Base.SetLattice.Top,_) -> true
            | Top (Base.SetLattice.Set _top_param,_orig) ->
              false
            | Map _ -> false);
        true

  let replace_base substitution v =
    let substitute replace make acc =
      let modified, set' = replace substitution acc in
      modified, if modified then make set' else v
    in
    match v with
    | Top (Base.SetLattice.Top, _) -> false, v
    | Top (Base.SetLattice.Set set, origin) ->
      substitute Base.Hptset.replace (inject_top_origin origin) set
    | Map map ->
      let decide _key  = Ival.join in
      substitute (M.replace_key ~decide) (fun m -> Map m) map

  let overlaps ~partial ~size mm1 mm2 =
    match mm1, mm2 with
    | Top _, _ | _, Top _ -> intersects mm1 mm2
    | Map m1, Map m2 ->
      (* The two locations may overlap if there are two offsets i1 and i2 such
         that |i1-i2| < size (and |i1-i2| > 0 when partial is true). *)
      let pred_size = Z.pred size in
      let min = if partial then Z.one else Z.zero in
      let size_itv = Ival.inject_range (Some min) (Some pred_size) in
      let decide_both _ x y =
        let abs_diff = Ival.abs_int (Ival.sub_int x y) in
        Ival.intersects abs_diff size_itv
      in
      M.symmetric_binary_predicate
        Hptmap_sig.NoCache M.ExistentialPredicate
        ~decide_fast:(fun _ _ -> M.PUnknown)
        ~decide_one:(fun _ _ -> false)
        ~decide_both
        m1 m2

  type widen_hint = Ival.widen_hint

  (* Computes widening thresholds according to the validity of [base]. *)
  let validity_widen_hints base =
    let zero = Z.Set.singleton Z.zero in
    let int_thresholds =
      match Base.validity base with
      | Base.Known (_, m)
      | Base.Unknown (_, _, m)
      | Base.Variable { Base.max_alloc = m } ->
        (* Try the frontier of the block: further accesses are invalid
           anyway. This also works great for constant strings (this computes
           the offset of the null terminator). *)
        let bound = Z.(pred (ediv (succ m) 8z)) in
        Z.Set.add bound zero
      | Base.Empty | Base.Invalid -> zero
    in
    int_thresholds, Datatype.Float.Set.empty

  let widen ?size ?hint =
    let widen_map =
      let decide base v1 v2 =
        let size, hint =
          if Base.is_null base then size, hint else
            (* Do not perform size-based widening for pointers. This will only
               delay convergence, for no real benefit. The only interesting
               bound is the validity. *)
            None, Some (validity_widen_hints base)
        in
        Ival.widen ?size ?hint v1 v2
      in
      M.join
        ~cache:Hptmap_sig.NoCache (* No cache, because of wh *)
        ~symmetric:false ~idempotent:true ~decide
    in
    fun m1 m2 ->
      match m1, m2 with
      | _ , Top _ -> m2
      | Top _, _ -> assert false (* m2 should be larger than m1 *)
      | Map m1, Map m2 -> Map (widen_map m1 m2)
end

module Bits = struct
  include Bytes

  let of_bytes x =
    map (Ival.scale (Bit_utils.sizeofchar())) x
  let to_bytes x =
    map (Ival.scale_div ~pos:true (Bit_utils.sizeofchar())) x
  let to_bytes_under x =
    map (Ival.scale_div_under ~pos:true (Bit_utils.sizeofchar())) x
end
