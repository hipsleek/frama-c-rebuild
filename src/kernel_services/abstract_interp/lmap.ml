(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Abstract_interp
open Lattice_bounds

type 'a default_contents =
  | Bottom
  | Top of 'a
  | Constant of 'a
  | Other


module type Default_offsetmap = sig
  type v
  type offsetmap

  val name: string
  val default_offsetmap : Base.t -> offsetmap Lattice_bounds.or_bottom
  val default_contents: v default_contents
end

module Make_LOffset
    (V: sig
       include Offsetmap_lattice_with_isotropy.S
       include Lattice_type.With_Top_Opt with type t := t
     end)
    (Offsetmap: Offsetmap_sig.S
     with type v = V.t
      and type widen_hint = V.widen_hint)
    (Default_offsetmap: Default_offsetmap with type v := V.t
                                           and type offsetmap := Offsetmap.t)
=
struct

  type v = V.t
  type offsetmap = Offsetmap.t

  type widen_hint_base = V.widen_hint

  open Default_offsetmap

  (* to be used only when we are sure that the base is not Invalid, for
     example because it is bound in at least one map. *)
  let default_bound_offsetmap b =
    match Default_offsetmap.default_offsetmap b with
    | `Bottom -> assert false
    | `Value o -> o

  let () =
    match Default_offsetmap.default_contents with
    | Constant v ->
      if not (V.is_isotropic v) then
        Kernel.fatal "[Lmap] invalid default contents for offsetmaps %a \
                      (datatype: %s)" V.pretty v V.datatype_name
    | _ -> ()

  let vtop () =
    match V.top_opt with
    | None -> raise Error_Top
    | Some v -> v

  module M =  struct

    module Offsetmap = struct
      include Offsetmap
      let datatype_name = Offsetmap.datatype_name ^ " " ^ Default_offsetmap.name
    end

    module Comp = struct
      let empty = true
      let leaf _base offsetmap = Offsetmap.cardinal_zero_or_one offsetmap
      let compose a b = a && b
    end

    module Info = struct
      let initial_values = []
      let dependencies = [ Ast.self ]
    end

    module M =
      Hptmap.Make_with_compositional_bool (Base.Base) (Offsetmap) (Comp) (Info)
    let () = Ast.add_monotonic_state M.self

    include M

    let add =
      match Default_offsetmap.default_contents with
      | Bottom -> fun b o m -> add b o m
      | Top top ->
        fun b o m ->
          if Offsetmap.is_same_value o top then remove b m else add b o m
      | Constant v ->
        fun b o m ->
          if Offsetmap.is_same_value o v then remove b m else add b o m
      | Other ->
        fun b o m ->
          let o' = default_bound_offsetmap b in
          if Offsetmap.equal o o' then remove b m else add b o m

    let find_or_default b map =
      try `Value (find b map)
      with Not_found -> default_offsetmap b

    let find_base = find

    let pretty fmt m =
      Pretty_utils.pp_iter ~pre:"@[<v>" ~sep:"@ " ~suf:"@]"
        (fun f -> iter (fun base offset -> f (base, offset)))
        (fun fmt (base, offs) ->
           let typ = Base.typeof base in
           Format.fprintf fmt "@[%a@[%a@]@]" Base.pretty base
             (Offsetmap.pretty_generic ?typ ()) offs)
        fmt
        m

    (* TODO: can be improved in quite a few ways. *)
    let filter_base f m =
      fold (fun k v acc -> if f k then add k v acc else acc) m empty

    let add_base_value base ~size v ~size_v m =
      add base (Offsetmap.create ~size v ~size_v) m

    exception Result_is_top

    let add_binding ~exact mem (loc: Locations.t) v =
      let had_non_bottom = ref false in
      let result = ref mem in
      let aux origin b offsets =
        let validity = Base.validity b in
        match find_or_default b mem with
        | `Bottom -> ()
        | `Value offm ->
          let offm' =
            match loc.size with
            | `Top ->
              let orig = Origin.current Origin.Misalign_write in
              Offsetmap.update_imprecise_everywhere ~validity orig v offm
            | `Value size ->
              assert (Z.geq size Z.zero);
              Offsetmap.update ?origin ~validity ~exact ~offsets ~size v offm
          in
          match offm' with
          | `Bottom -> ()
          | `Value offm' ->
            had_non_bottom := true;
            if offm != offm' then result := add b offm' !result
      in
      match loc.addr with
      | Addresses.Bits.Top (Base.SetLattice.Top, orig) ->
        Abstract_interp.feedback_approximation
          "writing at a completely unknown address @[%a@]"
          Origin.pretty_as_reason orig;
        raise Result_is_top
      | Addresses.Bits.Top (Base.SetLattice.Set set, origin) ->
        Base.Hptset.iter (fun b -> aux (Some origin) b Ival.top) set;
        !had_non_bottom, !result
      | Addresses.Bits.Map loc_map ->
        Addresses.Bits.M.iter (fun b off -> aux None b off) loc_map;
        !had_non_bottom, !result

    (* may raise Error_Top in the case Top Top. Make sure to annotate callers *)
    let find ?(conflate_bottom=true) mem (loc: Locations.t) =
      let open Bottom.Operators in
      let handle_imprecise_base base acc =
        let v =
          let* offsetmap = find_or_default base mem in
          Offsetmap.find_imprecise_everywhere offsetmap
        in
        Bottom.join V.join v acc
      in
      match loc.addr with
      | Addresses.Bits.Top (Base.SetLattice.Top, _) -> `Value (vtop ())
      | Addresses.Bits.Top (Base.SetLattice.Set s, _) ->
        Base.SetLattice.O.fold handle_imprecise_base s `Bottom
      | Addresses.Bits.Map loc_map -> begin
          match loc.size with
          | `Top ->
            let aux base _ acc = handle_imprecise_base base acc in
            Addresses.Bits.M.fold aux loc_map `Bottom
          | `Value size ->
            let aux_base base offsets acc_v =
              let validity = Base.validity base in
              match find_or_default base mem with
              | `Bottom -> acc_v
              | `Value offsetmap ->
                let new_v =
                  Offsetmap.find
                    ~conflate_bottom ~validity ~offsets ~size offsetmap
                in
                Bottom.join V.join new_v acc_v
            in
            Addresses.Bits.M.fold aux_base loc_map `Bottom
        end

    (* Internal function for join and widen, that handles efficiently the
       values bound by default in maps. *)
    let join_widen op =
      let cache = match op with
        | `Join -> Hptmap_sig.PersistentCache "lmap.join"
        | `Widen None -> Hptmap_sig.PersistentCache "lmap.widen"
        | `Widen Some _ -> Hptmap_sig.NoCache
      in
      let symmetric = match op with `Join -> true | `Widen _ -> false in
      let op = match op with
        | `Join -> fun _b o1 o2 -> Offsetmap.join o1 o2
        | `Widen None -> fun _b o1 o2 -> Offsetmap.widen o1 o2
        | `Widen (Some wh) -> fun b o1 o2 -> Offsetmap.widen ~hint:(wh b) o1 o2
      in
      let idempotent = true in
      let default = default_bound_offsetmap in
      let decide_both_revert default b o1 o2 =
        let o = op b o1 o2 in
        if Offsetmap.is_same_value o default then None else Some o
      in
      match Default_offsetmap.default_contents with
      | Bottom ->
        (* Missing keys are neutral w.r.t. merge because they are Bottom.
           Values cannot revert to default (bottom). *)
        join ~cache ~symmetric ~idempotent ~decide:op
      | Top top ->
        (* Join with a missing key returns Top, hence the default: we can use
           [inter]. Values can revert to the default through [op]. *)
        let decide b o1 o2 = decide_both_revert top b o1 o2 in
        inter ~cache ~symmetric ~idempotent ~decide
      | Constant v ->
        (* Missing keys must be treated one by one.
           Values can revert to default *)
        let decide_both b o1 o2 = decide_both_revert v b o1 o2 in
        let decide_left  b o = decide_both_revert v b o (default b) in
        let decide_right b o = decide_both_revert v b (default b) o in
        merge ~cache ~symmetric ~idempotent ~decide_both
          ~decide_left:(Traversing decide_left)
          ~decide_right:(Traversing decide_right)
      | Other ->
        (* Same idea as VCst *)
        let decide_two b default o1 o2 =
          let o = op b o1 o2 in
          if Offsetmap.equal o default then None else Some o
        in
        let decide_both b o1 o2 = decide_two b (default b) o1 o2 in
        let decide_left b o =
          let default = default b in decide_two b default o default
        in
        let decide_right b o =
          let default = default b in decide_two b default default o
        in
        merge ~cache ~symmetric ~idempotent ~decide_both
          ~decide_left:(Traversing decide_left)
          ~decide_right:(Traversing decide_right)

    let join = join_widen `Join

    module Make_Narrow (X: sig
        include Lattice_type.With_Top with type t := V.t
        include Lattice_type.With_Narrow with type t := V.t
        val bottom: V.t
      end) = struct

      exception NarrowReturnsBottom

      module OffsetmapNarrow = Offsetmap.Make_Narrow(struct
          let top = X.top

          (* Catch results of [narrow] that are bottom and raise an exception
             instead. *)
          let narrow x y =
            let r = X.narrow x y in
            if V.equal X.bottom r then raise NarrowReturnsBottom;
            r
        end)

      (* may raise {!NarrowReturnsBottom} when Bottom is strict *)
      let narrow_internal =
        let cache = Hptmap_sig.PersistentCache "lmap.narrow"
        and symmetric = true
        and idempotent = true in
        match Default_offsetmap.default_contents with
        | Bottom ->
          (* Bases completely mapped to Bottom disappear from the result, but we
             do *not* raise NarrowReturnsBottom (otherwise, we would never be
             able to call narrow with two different sets of variables). *)
          let decide b o1 o2 =
            let o = OffsetmapNarrow.narrow o1 o2 in
            let default = default_bound_offsetmap b in
            if Offsetmap.equal o default then None else Some o
          in
          M.inter ~cache~symmetric ~idempotent ~decide
        | Top _ ->
          (* Missing keys are implicitly bound to Top, hence neutral for the
             operation. Hence, we can use join. No need to check if a variable
             reverts to default, because narrow only decreases *)
          let decide _k v1 v2 = OffsetmapNarrow.narrow v1 v2 in
          M.join ~cache ~symmetric ~idempotent ~decide
        | Constant _ | Other ->
          (* No special optimisation, we perform a pointwise narrow and see if
             we revert to the default value. [`Constant] case could be improved.
             Since [narrow] is symmetric, [decide_left == decide_right].
             Otherwise, see [`Other] case in {!join_widen} *)
          let decide_two default o1 o2 =
            let o = OffsetmapNarrow.narrow o1 o2 in
            if Offsetmap.equal o default then None else Some o
          in
          let decide_one b o =
            let default = default_bound_offsetmap b in
            decide_two default o default
          in
          let decide_both b o1 o2 =
            decide_two (default_bound_offsetmap b) o1 o2
          in
          let decide_left = Traversing decide_one in
          merge ~cache ~symmetric ~idempotent
            ~decide_both ~decide_left ~decide_right:decide_left

    end

    let merge =
      let cache = Hptmap_sig.NoCache in
      let decide _ _v1 v2 = v2 in
      M.join ~cache ~symmetric:false ~idempotent:true ~decide

    let pretty_diff_aux fmt m1 m2 =
      let print base m1 m2 = match m1, m2 with
        | None, None -> ()
        | Some m, None ->
          let typ = Base.typeof base in
          Format.fprintf fmt "@[L %a@[%a@]@]@ "
            Base.pretty base (Offsetmap.pretty_generic ?typ ()) m
        | None, Some m ->
          let typ = Base.typeof base in
          Format.fprintf fmt "@[R %a@[%a@]@]@ "
            Base.pretty base (Offsetmap.pretty_generic ?typ ()) m
        | Some m1, Some m2 ->
          if not (Offsetmap.equal m1 m2) then
            let typ = Base.typeof base in
            let pp = Offsetmap.pretty_generic ?typ () in
            Format.fprintf fmt "@[%a @[<v>L@[%a@]@,R@[%a@]@]@]@ "
              Base.pretty base pp m1 pp m2
      in
      (* The diff is implemented by performing a merge on the two offsetmaps.
         We _must not_ cache the result, as we are interested only in the side
         effects. *)
      let decide b m1 m2 =
        print b m1 m2;
        (match m1, m2 with (* We need something to return *)
         | Some m, _ | _, Some m -> m
         | None, None -> assert false (* generic_merge invariant *))
      in
      let aux =
        generic_join ~cache:Hptmap_sig.NoCache
          ~idempotent:true ~symmetric:false ~decide
      in
      Format.fprintf fmt "@[<v>";
      ignore (aux m1 m2);
      Format.fprintf fmt "@]"

    let pretty_diff fmt m1 m2 =
      if m1 == m2 then Format.fprintf fmt "Equal"
      else pretty_diff_aux fmt m1 m2

    let is_included =
      let name = Format.asprintf "Lmap(%s).is_included" V.datatype_name in
      let decide_fst base v1 =
        Offsetmap.is_included v1 (default_bound_offsetmap base)
      in
      let decide_snd base v2 =
        Offsetmap.is_included (default_bound_offsetmap base) v2
      in
      let decide_both _ m1 m2 = Offsetmap.is_included m1 m2 in
      let decide_fast =
        match Default_offsetmap.default_contents with
        | Bottom ->
          (fun s t ->
             if s == t || is_empty s (*all bases present in t but not in s
                                       are implicitly bound to Bottom in s, hence the inclusion holds *)
             then PTrue
             else PUnknown)
        | Top _ ->
          (fun s t ->
             if s == t || is_empty t (*all bases present in s but not in t
                                       are implicitly bound to Top in t, hence the inclusion holds *)
             then PTrue
             else PUnknown)
        | _ -> (fun s t -> if s == t then PTrue else PUnknown)
      in
      binary_predicate
        (Hptmap_sig.PersistentCache name) UniversalPredicate
        ~decide_fast ~decide_fst ~decide_snd ~decide_both

    type widen_hint = Base.t -> V.widen_hint

    (* Precondition : m1 <= m2 *)
    let widen ?(priority=Base.Set.empty) ?hint m1 m2 =
      let widened, something_done =
        Base.Set.fold
          (fun key (widened, something_done) ->
             let offs2 = find_or_default key m2 in
             let offs1 = find_or_default key m1 in
             match offs1, offs2 with
             | `Bottom, _ | _, `Bottom ->
               assert false (* cannot be invalid and bound *)
             | `Value offs1, `Value offs2 ->
               let unchanged = Offsetmap.equal offs2 offs1 in
               (* Format.printf "key=%a, fixed=%b@."
                      Base.pretty key fixed; *)
               if unchanged
               then (widened, something_done)
               else
                 let hint = Option.map (fun f -> f key) hint in
                 let new_off = Offsetmap.widen ?hint offs1 offs2 in
                 (add key new_off widened, true)
          ) priority (m2, false)
      in
      if something_done then
        widened
      else
        let r = join_widen (`Widen hint) m1 m2 in
        (* If [r] is equal to [m2], the widening had no effect.
           If [m1] was not equal to [m2], either [m2] has reached some widening
           threshold (and the widening is postponed), or there is a convergence
           issue, for instance if the size of an allocated base is increased at
           each loop iteration. To avoid such issue, we widen the size of weak
           bases whose offsetmaps have changed between [m1] and [m2]. *)
        if equal r m2 && not (equal m1 m2) then begin
          let update_weak_base_validity base o1 o2 =
            if Base.is_weak base && not (Offsetmap.equal o1 o2) then
              match Base.validity base with
              | Base.Variable v when Z.lt v.max_alloc v.max_allocable ->
                (* Increasing [max_alloc] is never unsound as any access beyond
                   [min_alloc] will generate an alarm anyway. *)
                Base.update_variable_validity v ~weak:true
                  ~min_alloc:v.min_alloc ~max_alloc:v.max_allocable
              | _ -> ()
          in
          fold2_join_heterogeneous
            ~cache:Hptmap_sig.NoCache
            ~join:(fun () () -> ()) ~empty:()
            ~empty_left:(fun _ -> ()) ~empty_right:(fun _ -> ())
            ~both:update_weak_base_validity
            m1 m2
        end;
        r

    let paste_offsetmap ~from ~dst_addr ~size ~exact m =
      let loc_dst = Locations.make dst_addr (`Value size) in
      assert (Z.leq Z.zero size);
      let exact = exact && Locations.cardinal_zero_or_one loc_dst in
      (* TODO: do we want to alter exact here? *)
      let had_non_bottom = ref false in
      let treat_dst base_dst i_dst acc =
        let validity = Base.validity base_dst in
        let offsetmap_dst = find_or_default base_dst m in
        match offsetmap_dst with
        | `Bottom -> acc
        | `Value offsetmap_dst ->
          let new_offsetmap =
            Offsetmap.paste_slice ~validity ~exact
              ~from ~size ~offsets:i_dst offsetmap_dst
          in
          had_non_bottom := true;
          match new_offsetmap with
          | `Bottom -> acc
          | `Value new_offsetmap ->
            if offsetmap_dst != new_offsetmap then
              add base_dst new_offsetmap acc
            else acc
      in
      match dst_addr with
      | Addresses.Bits.Map _ ->
        let result = Addresses.Bits.fold_i treat_dst dst_addr m in
        !had_non_bottom, result
      | Addresses.Bits.Top (top, orig) ->
        if not (Base.SetLattice.equal top Base.SetLattice.top) then
          Abstract_interp.feedback_approximation
            "writing somewhere in @[%a@]@[%a@]."
            Base.SetLattice.pretty top
            Origin.pretty_as_reason orig;
        let validity = Base.validity_from_size size in
        match Offsetmap.find_imprecise ~validity from with
        | `Bottom -> false, m
        | `Value v -> add_binding ~exact:false m loc_dst v

    let top_offsetmap size =
      let top = vtop () in
      `Value (Offsetmap.create_isotropic ~size top)

    (* may raise Error_Top in the case Top Top *)
    let copy_offsetmap src_loc size m =
      let treat_src k_src i_src acc =
        let validity = Base.validity k_src in
        match find_or_default k_src m with
        | `Bottom -> acc
        | `Value offsetmap_src ->
          let copy =
            Offsetmap.copy_slice ~validity~offsets:i_src ~size offsetmap_src
          in
          Bottom.join Offsetmap.join acc copy
      in
      try
        Addresses.Bits.fold_topset_ok treat_src src_loc `Bottom
      with Error_Top -> top_offsetmap size

  end

  type map = M.t
  let shape x = x
  let clear_caches = M.clear_caches

  exception Found_prefix = M.Found_prefix

  type lmap =
    | Bottom
    | Top
    | Map of M.t

  let equal m1 m2 =
    match m1, m2 with
    | Bottom, Bottom -> true
    | Top, Top -> true
    | Map m1, Map m2 -> M.equal m1 m2
    | (Bottom | Top | Map _), _ -> false

  let comp_prefixes m1 m2 =
    match m1, m2 with
    | Map m1, Map m2 -> M.comp_prefixes m1 m2
    | _ -> ()

  type subtree = M.subtree
  let find_prefix m p =
    match m with
    | Map m -> M.find_prefix m p
    | Top | Bottom -> None
  let equal_subtree = M.equal_subtree
  let hash_subtree = M.hash_subtree

  let compare =
    if M.compare == Datatype.undefined then Datatype.undefined
    else
      fun m1 m2 -> match m1, m2 with
        | Bottom, Bottom | Top, Top -> 0
        | Map m1, Map m2 -> M.compare m1 m2
        | Bottom, (Top | Map _) | Top, Map _ -> -1
        | Map _, (Top | Bottom) | Top, Bottom -> 1

  let empty_map = Map M.empty

  let hash = function
    | Bottom -> 457
    | Top -> 458
    | Map m -> M.hash m

  let pretty fmt m =
    match m with
    | Bottom -> Format.fprintf fmt "@[NOT ACCESSIBLE@]"
    | Top -> Format.fprintf fmt "@[NO INFORMATION@]"
    | Map m -> M.pretty fmt m

  let pretty_debug fmt m =
    match m with
    | Top | Bottom -> pretty fmt m
    | Map m -> M.pretty_debug fmt m

  include Datatype.Make_with_collections
      (struct
        type t = lmap
        let structural_descr =
          Structural_descr.t_sum [| [| M.packed_descr |] |]
        let name =
          Printf.sprintf "(%s, %s) Lmap"
            Offsetmap.datatype_name Default_offsetmap.name
        let reprs = Bottom :: Top :: List.map (fun b -> Map b) M.reprs
        let equal = equal
        let compare = compare
        let hash = hash
        let pretty = pretty
        let rehash = Datatype.identity
        let copy = Datatype.undefined
        let mem_project = Datatype.never_any_project
      end)

  let top = Top
  let bottom = Bottom
  let is_top x = equal top x

  let add_base base offsetmap acc =
    match acc with
    | Map acc -> Map (M.add base offsetmap acc)
    | Bottom -> Bottom
    | Top -> Top

  let is_empty_map = function
    | Bottom -> false
    | Top -> false
    | Map m -> M.is_empty m

  let filter_base f = function
    | Top -> Top
    | Bottom -> Bottom
    | Map m -> Map (M.filter_base f m)

  let filter_by_shape shape = function
    | Top -> Top
    | Bottom -> Bottom
    | Map m -> Map (M.inter_with_shape shape m)

  let find_base b = function
    | Bottom -> `Bottom
    | Top -> `Top
    | Map m -> `Value (M.find_base b m)

  let find_base_or_default b = function
    | Bottom -> `Bottom
    | Top -> `Top
    | Map m -> (M.find_or_default b m :> _ or_top_bottom)

  let remove_base b = function
    | Bottom -> Bottom
    | Top -> Top
    | Map m -> Map (M.remove b m)

  let replace_base shape = function
    | Bottom -> Bottom
    | Top -> Top
    | Map map as t ->
      let decide _key = Offsetmap.join in
      let modified, map = M.replace_key ~decide shape map in
      if modified then Map map else t

  let is_reachable = function
    | Bottom -> false
    | Top | Map _ -> true

  let add_base_value base ~size v ~size_v = function
    | Bottom -> Bottom
    | Top -> Top
    | Map m -> Map (M.add_base_value base ~size v ~size_v m)

  let add_binding ~exact m loc v =
    match m with
    | Top -> Top
    | Bottom -> Bottom
    | Map m ->
      try
        let non_bottom, r = M.add_binding ~exact m loc v in
        if non_bottom then Map r else Bottom
      with M.Result_is_top -> Top

  let find ?(conflate_bottom=true) m loc =
    match m with
    | Bottom -> `Bottom
    | Top -> `Value (vtop ())
    | Map m -> M.find ~conflate_bottom m loc

  let join mm1 mm2 = match mm1, mm2 with
    | Bottom,m | m,Bottom -> m
    | Top, _ | _, Top -> Top
    | Map m1, Map m2 ->
      if M.equal m1 m2 then mm1 else Map (M.join m1 m2)

  module Make_Narrow (X: sig
      include Lattice_type.With_Top with type t := v
      include Lattice_type.With_Narrow with type t := v
      val bottom: v
    end) = struct

    include M.Make_Narrow(X)

    let narrow mm1 mm2 =
      match mm1, mm2 with
      | Bottom,_ | _,Bottom -> Bottom
      | Top, m | m, Top -> m
      | Map m1, Map m2 ->
        if M.equal m1 m2 then mm1
        else
          try Map (narrow_internal m1 m2)
          with NarrowReturnsBottom -> Bottom
  end

  let merge ~into t = match into, t with
    | Bottom, _ | _, Bottom
    | Top, _ | _, Top -> t
    | Map into, Map m -> Map (M.merge into m)

  let pretty_diff fmt m1 m2 =
    match m1, m2 with
    | Bottom, _ -> Format.fprintf fmt "BOT / Not BOT"
    | _, Bottom -> Format.fprintf fmt "Not BOT / BOT"
    | Top, _ -> Format.fprintf fmt "TOP / Not TOP"
    | _, Top -> Format.fprintf fmt "Not TOP / TOP"
    | Map m1, Map m2 -> M.pretty_diff fmt m1 m2

  let is_included m1 m2 =
    match m1, m2 with
    | Bottom,_ -> true | _,Bottom -> false
    |   _, Top -> true | Top, _ -> false
    | Map m1, Map m2 -> M.is_included m1 m2

  type widen_hint = M.widen_hint

  (* Precondition : m1 <= m2 *)
  let widen ?priority ?hint r1 r2 =
    match r1,r2 with
    | Top, Top | _, Top -> Top
    | Bottom,Bottom -> Bottom
    | _, Bottom | Top, Map _-> assert false (* thanks to precondition *)
    | Bottom, m -> m
    | Map m1,Map m2 -> Map (M.widen ?priority ?hint m1 m2)

  let paste_offsetmap ~from ~dst_addr ~size ~exact m =
    match m with
    | Bottom -> m
    | Top -> m
    | Map m ->
      try
        let non_bottom, r = M.paste_offsetmap ~from ~dst_addr ~size ~exact m in
        if non_bottom then Map r else Bottom
      with M.Result_is_top -> Top

  let copy_offsetmap src_loc size m =
    match m with
    | Bottom -> `Bottom
    | Top -> M.top_offsetmap size
    | Map m -> M.copy_offsetmap src_loc size m

  let fold = M.fold

  let iter = M.iter

  let cached_fold = M.cached_fold

  let cached_map ~f ~cache ~temporary =
    let cached_f = M.cached_map ~f ~cache ~temporary in
    function
    | Top -> Top
    | Bottom -> Bottom
    | Map m -> Map (cached_f m)

  let remove_variables vars state =
    let cleanup acc v = remove_base (Base.of_varinfo v) acc in
    List.fold_left cleanup state vars

end
