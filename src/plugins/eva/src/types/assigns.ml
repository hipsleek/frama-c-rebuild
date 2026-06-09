(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module DepsOrUnassigned = struct
  module Datatype_Input = struct
    include Datatype.Serializable_undefined

    type t =
      | Unassigned
      | AssignedFrom of Deps.t
      | MaybeAssignedFrom of Deps.t
    [@@deriving eq,ord]

    let name = "Eva.Froms.DepsOrUnassigned"

    let pretty fmt = function
      | Unassigned -> Format.pp_print_string fmt "UNASSIGNED"
      | AssignedFrom fd -> Deps.pretty_precise fmt fd
      | MaybeAssignedFrom fd ->
        Format.fprintf fmt "%a (or UNASSIGNED)" Deps.pretty_precise fd

    let hash = function
      | Unassigned -> 1
      | AssignedFrom fd -> Hashtbl.hash (2, Deps.hash fd)
      | MaybeAssignedFrom fd -> Hashtbl.hash (3, Deps.hash fd)

    let reprs = Unassigned :: List.map (fun r -> AssignedFrom r) Deps.reprs
  end

  include Datatype.Make (Datatype_Input)
  include Datatype_Input

  let join d1 d2 = match d1, d2 with
    | Unassigned, Unassigned -> Unassigned
    | Unassigned, AssignedFrom fd | AssignedFrom fd, Unassigned ->
      MaybeAssignedFrom fd
    | Unassigned, (MaybeAssignedFrom _ as d)
    | (MaybeAssignedFrom _ as d), Unassigned ->
      d
    | AssignedFrom fd1, AssignedFrom fd2 ->
      AssignedFrom (Deps.join fd1 fd2)
    | AssignedFrom fd1, MaybeAssignedFrom fd2
    | MaybeAssignedFrom fd1, AssignedFrom fd2
    | MaybeAssignedFrom fd1, MaybeAssignedFrom fd2 ->
      MaybeAssignedFrom (Deps.join fd1 fd2)

  let is_included d1 d2 = match d1, d2 with
    | Unassigned, (Unassigned | AssignedFrom _ | MaybeAssignedFrom _) ->
      true
    | MaybeAssignedFrom fd1, (AssignedFrom fd2 | MaybeAssignedFrom fd2)
    | AssignedFrom fd1, AssignedFrom fd2 ->
      Deps.is_included fd1 fd2
    | (AssignedFrom _ | MaybeAssignedFrom _), Unassigned
    | AssignedFrom _, MaybeAssignedFrom _ ->
      false

  let top = MaybeAssignedFrom Deps.top
  let default = Unassigned
  let default_is_bottom = false

  let to_zone = function
    | Unassigned -> Memory_zone.bottom
    | AssignedFrom fd | MaybeAssignedFrom fd -> Deps.to_zone fd

  let may_be_unassigned = function
    | AssignedFrom _ -> false
    | Unassigned | MaybeAssignedFrom _ -> true
end

module Memory = struct
  (* A From table is internally represented as a Lmap of [DepsOrUnassigned].
     However, the API mostly hides this fact, and exports access functions
     that take or return [Deps.t] values. This way, the user needs not
     understand the subtleties of Unassigned/MaybeAssigned. *)

  include Lmap_bitwise.Make_bitwise(DepsOrUnassigned)

  let add_binding_precise_loc ~exact access m loc v =
    let aux_one_loc loc m =
      let loc = Locations.valid_part access loc in
      add_binding_loc ~exact m loc (DepsOrUnassigned.AssignedFrom v)
    in
    Precise_locs.fold aux_one_loc loc m

  let add_binding ~exact m z v =
    add_binding ~exact m z (DepsOrUnassigned.AssignedFrom v)

  let add_binding_loc ~exact m loc v =
    add_binding_loc ~exact m loc (DepsOrUnassigned.AssignedFrom v)

  (* This is the auxiliary datastructure used to write the function [find].
     When we iterate over a offsetmap of value [DepsOrUnassigned], we obtain
     two things: (1) some dependencies; (2) some intervals that may have not
     been assigned, and that will appear as data dependencies (once we know
     the base we are iterating on). *)
  type find_offsm = {
    fo_itvs: Int_Intervals.t;
    fo_deps: Deps.t;
  }

  (* Once the base is known, we can obtain something of type [Deps.t] *)
  let convert_find_offsm base fp =
    let z = Memory_zone.inject base fp.fo_itvs in
    Deps.add_data fp.fo_deps z

  let empty_find_offsm = {
    fo_itvs = Int_Intervals.bottom;
    fo_deps = Deps.bottom;
  }

  let join_find_offsm fp1 fp2 =
    if fp1 == empty_find_offsm then fp2
    else if fp2 == empty_find_offsm then fp1
    else {
      fo_itvs = Int_Intervals.join fp1.fo_itvs fp2.fo_itvs;
      fo_deps = Deps.join fp1.fo_deps fp2.fo_deps;
    }

  (* Auxiliary function that collects the dependencies on some intervals of
     an offsetmap. *)
  let find_precise_offsetmap : Int_Intervals.t -> LOffset.t -> find_offsm =
    let cache = Hptmap_sig.PersistentCache "Eva.Froms.Memory.find_precise" in
    let aux_find_offsm ib ie v =
      (* If the interval can be unassigned, we collect its bound. We also
         return the dependencies stored at this interval. *)
      let default, v = match v with
        | DepsOrUnassigned.Unassigned -> true, Deps.bottom
        | DepsOrUnassigned.MaybeAssignedFrom v -> true, v
        | DepsOrUnassigned.AssignedFrom v -> false, v
      in
      { fo_itvs =
          if default
          then Int_Intervals.inject_bounds ib ie
          else Int_Intervals.bottom;
        fo_deps = v }
    in
    (* Partial application is important *)
    LOffset.fold_join_itvs
      ~cache aux_find_offsm join_find_offsm empty_find_offsm

  (* Collecting dependencies on a given zone. *)
  let find_precise : t -> Memory_zone.t -> Deps.t =
    let both = find_precise_offsetmap in
    let conv = convert_find_offsm in
    (* We are querying a zone for which no dependency is stored. Hence, every
       base is implicitly bound to [Unassigned]. *)
    let empty_map z = Deps.data z in
    let join = Deps.join in
    let empty = Deps.bottom in
    (* Partial application is important *)
    let f = fold_join_zone ~both ~conv ~empty_map ~join ~empty in
    fun m z ->
      match m with
      | Top -> Deps.top
      | Bottom -> Deps.bottom
      | Map m -> try f z m with Abstract_interp.Error_Top -> Deps.top

  let find_precise_loffset loffset base itv =
    let fo = find_precise_offsetmap itv loffset in
    convert_find_offsm base fo

  let find z m = Deps.to_zone (find_precise z m)
end

module Datatype_Input = struct
  include Datatype.Serializable_undefined

  type t = {
    return : Deps.t;
    memory : Memory.t
  }
  [@@deriving eq,ord]

  let name = "Eva.Froms"

  let top = {
    return = Deps.top;
    memory = Memory.top;
  }

  let reprs = [ top ]

  let hash assigns =
    Hashtbl.hash (Memory.hash assigns.memory, Deps.hash assigns.return)

  let pretty fmt assigns =
    Format.fprintf fmt "%a@\n\\result FROM @[%a@]@\n"
      Memory.pretty assigns.memory
      Deps.pretty assigns.return
end

include Datatype.Make (Datatype_Input)
include Datatype_Input

let join x y =
  {
    return = Deps.join x.return y.return;
    memory = Memory.join x.memory y.memory;
  }

let outputs assigns =
  match assigns.memory with
  | Memory.Top -> Memory_zone.top
  | Memory.Bottom -> Memory_zone.bottom
  | Memory.Map m ->
    Memory.fold
      (fun z v acc ->
         let open DepsOrUnassigned in
         match v with
         | Unassigned -> acc
         | AssignedFrom _ | MaybeAssignedFrom _ -> Memory_zone.join z acc)
      m Memory_zone.bottom
