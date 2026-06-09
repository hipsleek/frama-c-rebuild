(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Deps = Eva.Deps

module DepsOrUnassigned = struct
  include Eva.Assigns.DepsOrUnassigned

  let subst f d =
    match d with
    | Unassigned -> Unassigned
    | AssignedFrom fd ->
      let fd' = f fd in
      if fd == fd' then d else AssignedFrom fd'
    | MaybeAssignedFrom fd ->
      let fd' = f fd in
      if fd == fd' then d else MaybeAssignedFrom fd'

  let compose d1 d2 =
    match d1, d2 with
    | Unassigned, _ -> d2
    | AssignedFrom _, _ -> d1
    | MaybeAssignedFrom _, Unassigned -> d1
    | MaybeAssignedFrom d1, MaybeAssignedFrom d2 ->
      MaybeAssignedFrom (Deps.join d1 d2)
    | MaybeAssignedFrom d1, AssignedFrom d2 ->
      AssignedFrom (Deps.join d1 d2)

  (* for backwards compatibility *)
  let pretty fmt fd =
    match fd with
    | Unassigned -> Format.pp_print_string fmt "(SELF)"
    | AssignedFrom d -> Memory_zone.pretty fmt (Deps.to_zone d)
    | MaybeAssignedFrom d ->
      Format.fprintf fmt "%a (and SELF)" Memory_zone.pretty (Deps.to_zone d)

  let pretty_precise fmt = function
    | Unassigned -> Format.pp_print_string fmt "UNASSIGNED"
    | AssignedFrom fd -> Deps.pretty_precise fmt fd
    | MaybeAssignedFrom fd ->
      (* '(or UNASSIGNED)' would be a better pretty-printer, we use
         '(and SELF)' only for compatibility reasons *)
      Format.fprintf fmt "%a (and SELF)" Deps.pretty_precise fd
end

include Eva.Assigns.Memory

let bind_var vi v m =
  let z = Locations.zone_of_varinfo vi in
  add_binding ~exact:true m z v

let unbind_var vi m =
  remove_base (Base.of_varinfo vi) m

let map f = map (DepsOrUnassigned.subst f)

let is_unassigned m =
  LOffset.is_same_value m DepsOrUnassigned.Unassigned

(* Unassigned is a neutral value for compose, on both sides *)
let decide_compose m1 m2 =
  if m1 == m2 || is_unassigned m1 then LOffset.ReturnRight
  else if is_unassigned m2 then LOffset.ReturnLeft
  else LOffset.Recurse

let compose_map =
  let cache = Hptmap_sig.PersistentCache "From_memory.compose" in
  (* Partial application is important because of the cache. Idempotent,
     because [compose x x] is always equal to [x]. *)
  map2 ~cache ~symmetric:false ~idempotent:true ~empty_neutral:true
    decide_compose DepsOrUnassigned.compose

let compose m1 m2 = match m1, m2 with
  | Top, _ | _, Top -> Top
  | Map m1, Map m2 -> Map (compose_map m1 m2)
  | Bottom, (Map _ | Bottom) | Map _, Bottom -> Bottom


(* ----- Substitute --------------------------------------------------------- *)

(** Auxiliary function that substitutes the data right-hand part of a
    dependency by a pre-existing From state. The returned result is a Deps.t:
    the data part will be the data part of the complete result, the indirect
    part will be added to the indirect part of the final result. *)
(* This function iterates simultaneously on a From memory, and on a zone.
   It is cached. The definitions below are used to call the function that
   does the recursive descent. *)
let substitute_data_deps =
  (* Nothing left to substitute, return z unchanged *)
  let empty_right z = Deps.data z in
  (* Zone to substitute is empty *)
  let empty_left _ = Deps.bottom in
  (* [b] is in the zone and substituted. Rewrite appropriately *)
  let both b itvs offsm = find_precise_loffset offsm b itvs in
  let join = Deps.join in
  let empty = Deps.bottom in
  let cache = Hptmap_sig.PersistentCache "From_memory.substitute_data" in
  let f_map =
    Memory_zone.fold2_join_heterogeneous
      ~cache ~empty_left ~empty_right ~both ~join ~empty
  in
  fun call_site_froms z ->
    match call_site_froms with
    | Bottom -> Deps.bottom
    | Top -> Deps.top
    | Map m ->
      try f_map z (shape m)
      with Abstract_interp.Error_Top -> Deps.top

(** Auxiliary function that substitutes the indirect right-hand part of a
    dependency by a pre-existing From state. The returned result is a zone,
    which will be added to the indirect part of the final result. *)
let substitute_indirect_deps =
  (* Nothing left to substitute, z is directly an indirect dependency *)
  let empty_right z = z in
  (* Zone to substitute is empty *)
  let empty_left _ = Memory_zone.bottom in
  let both b itvs offsm =
    (* Both the found data and indirect dependencies are computed for indirect
       dependencies: merge to a single zone *)
    Deps.to_zone (find_precise_loffset offsm b itvs)
  in
  let join = Memory_zone.join in
  let empty = Memory_zone.bottom in
  let cache = Hptmap_sig.PersistentCache "From_memory.substitute_indirect" in
  let f_map =
    Memory_zone.fold2_join_heterogeneous
      ~cache ~empty_left ~empty_right ~both ~join ~empty
  in
  fun call_site_froms z ->
    match call_site_froms with
    | Bottom -> Memory_zone.bottom
    | Top -> Memory_zone.top
    | Map m ->
      try f_map z (shape m)
      with Abstract_interp.Error_Top -> Memory_zone.top

let substitute call_site_froms deps =
  let open Deps in
  let { data; indirect } = deps in
  (* depending directly on an indirect dependency -> indirect,
     depending indirectly on a direct dependency  -> indirect *)
  let dirdeps = substitute_data_deps call_site_froms data in
  let inddeps = substitute_indirect_deps call_site_froms indirect in
  let dir = dirdeps.data in
  let ind = Memory_zone.(join dirdeps.indirect inddeps) in
  { data = dir; indirect = ind }


(* ----- Dependency of returned values -------------------------------------- *)

type return = Deps.t

let default_return = Deps.bottom

let top_return = Deps.top

let add_to_return ?start:(_start=0) ~size:_size ?(m=default_return) v =
  Deps.join m v
(*
    let start = Ival.of_int start in
    let itvs = Int_Intervals.from_ival_size start size in
    LOffset.add_iset ~exact:true itvs (DepsOrUnassigned.AssignedFrom v) m
*)

let top_return_size size =
  add_to_return ~size Deps.top

let collapse_return x = x


(* ----- Pretty-printing ---------------------------------------------------- *)

let pretty_skip = function
  | DepsOrUnassigned.Unassigned -> true
  | DepsOrUnassigned.AssignedFrom _ -> false
  | DepsOrUnassigned.MaybeAssignedFrom _ -> false

let pretty =
  pretty_generic_printer
    ~skip_v:pretty_skip ~pretty_v:DepsOrUnassigned.pretty ~sep:"FROM" ()

let pretty_ind_data =
  pretty_generic_printer
    ~skip_v:pretty_skip ~pretty_v:DepsOrUnassigned.pretty_precise ~sep:"FROM"
    ()

(** same as pretty, but uses the type of the function to output more
    precise information.
    @raise Error if the given type is not a function type
*)
let pretty_with_type ~indirect typ fmt assigns =
  let Eva.Assigns.{ memory; return } = assigns in
  let (rt_typ,_,_,_) = Cil.splitFunctionType typ in
  if is_bottom memory
  then Format.fprintf fmt
      "@[NON TERMINATING - NO EFFECTS@]"
  else
    let map_pretty =
      if indirect
      then pretty_ind_data
      else pretty
    in
    if Ast_types.is_void rt_typ
    then begin
      if is_empty memory
      then Format.fprintf fmt "@[NO EFFECTS@]"
      else map_pretty fmt memory
    end
    else
      let pp_space fmt =
        if not (is_empty memory) then
          Format.fprintf fmt "@ "
      in
      Format.fprintf fmt "@[<v>%a%t@[\\result FROM @[%a@]@]@]"
        map_pretty memory pp_space
        (if indirect then Deps.pretty_precise else Deps.pretty) return

let pretty_with_type_indirect = pretty_with_type ~indirect:true
let pretty_with_type = pretty_with_type ~indirect:false
