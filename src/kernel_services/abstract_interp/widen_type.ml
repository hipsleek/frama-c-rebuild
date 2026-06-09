(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_datatype

module IntSet = Z.Set
module FloatSet = Datatype.Float.Set

module Num_hints_stmt = Stmt.Map.Make(IntSet)
module Float_hints_stmt = Stmt.Map.Make(FloatSet)
module Num_hints_bases = Base.Map.Make(IntSet)
module Float_hints_bases = Base.Map.Make(FloatSet)
module Num_hints_bases_stmt = Stmt.Map.Make(Num_hints_bases)
module Float_hints_bases_stmt = Stmt.Map.Make(Float_hints_bases)
module Priority_bases_stmt = Stmt.Map.Make(Base.Set)

type widen_hints = {
  priority_bases: Base.Set.t Stmt.Map.t;
  default_hints: IntSet.t;
  default_float_hints: FloatSet.t;
  default_hints_by_stmt: IntSet.t Stmt.Map.t;
  default_float_hints_by_stmt: FloatSet.t Stmt.Map.t;
  hints_by_addr: IntSet.t Base.Map.t;
  float_hints_by_addr: FloatSet.t Base.Map.t;
  hints_by_addr_by_stmt: IntSet.t Base.Map.t Stmt.Map.t;
  float_hints_by_addr_by_stmt: FloatSet.t Base.Map.t Stmt.Map.t;
}

(* an [empty] set of hints *)
let empty = {
  priority_bases = Stmt.Map.empty;
  default_hints = IntSet.empty;
  default_float_hints = FloatSet.empty;
  default_hints_by_stmt = Stmt.Map.empty;
  default_float_hints_by_stmt = Stmt.Map.empty;
  hints_by_addr = Base.Map.empty;
  float_hints_by_addr = Base.Map.empty;
  hints_by_addr_by_stmt = Stmt.Map.empty;
  float_hints_by_addr_by_stmt = Stmt.Map.empty;
}

include Datatype.Make(struct
    include Datatype.Serializable_undefined
    type t = widen_hints
    let name = "Widen_type.widen_hints"
    let structural_descr =
      Structural_descr.t_tuple
        [| Priority_bases_stmt.packed_descr;
           IntSet.packed_descr;
           FloatSet.packed_descr;
           Num_hints_stmt.packed_descr;
           Float_hints_stmt.packed_descr;
           Num_hints_bases.packed_descr;
           Float_hints_bases.packed_descr;
           Num_hints_bases_stmt.packed_descr;
           Float_hints_bases_stmt.packed_descr |]
    let reprs =
      List.product_map
        (fun wh fh ->
           { priority_bases = Stmt.Map.empty;
             default_hints = wh;
             default_float_hints = fh;
             default_hints_by_stmt = Stmt.Map.empty;
             default_float_hints_by_stmt = Stmt.Map.empty;
             hints_by_addr = Base.Map.empty;
             float_hints_by_addr = Base.Map.empty;
             float_hints_by_addr_by_stmt = Stmt.Map.empty;
             hints_by_addr_by_stmt = Stmt.Map.empty
           })
        IntSet.reprs FloatSet.reprs
    let mem_project = Datatype.never_any_project
  end)

let join wh1 wh2 =
  let map_union s_join _key bs1 bs2 = Some (s_join bs1 bs2) in
  { priority_bases =
      Stmt.Map.union (map_union Base.Set.union)
        wh1.priority_bases wh2.priority_bases;
    default_hints =
      IntSet.union wh1.default_hints wh2.default_hints;
    default_float_hints =
      FloatSet.union wh1.default_float_hints wh2.default_float_hints;
    default_hints_by_stmt =
      Stmt.Map.union (map_union IntSet.union)
        wh1.default_hints_by_stmt wh2.default_hints_by_stmt;
    default_float_hints_by_stmt =
      Stmt.Map.union (map_union FloatSet.union)
        wh1.default_float_hints_by_stmt wh2.default_float_hints_by_stmt;
    hints_by_addr =
      Base.Map.union (map_union IntSet.union)
        wh1.hints_by_addr wh2.hints_by_addr;
    float_hints_by_addr =
      Base.Map.union (map_union FloatSet.union)
        wh1.float_hints_by_addr wh2.float_hints_by_addr;
    hints_by_addr_by_stmt =
      Stmt.Map.union
        (map_union (Base.Map.union (map_union IntSet.union)))
        wh1.hints_by_addr_by_stmt wh2.hints_by_addr_by_stmt;
    float_hints_by_addr_by_stmt =
      Stmt.Map.union
        (map_union (Base.Map.union (map_union FloatSet.union)))
        wh1.float_hints_by_addr_by_stmt wh2.float_hints_by_addr_by_stmt;
  }

let pretty fmt wh =
  let pp_bindings pp_key pp_elt fmt l =
    Format.fprintf fmt "%a"
      (Pretty_utils.pp_list ~sep:",@ "
         (Pretty_utils.pp_pair ~sep:" -> " pp_key pp_elt)) l
  in
  let pp_base_map pp_elt fmt m =
    Format.fprintf fmt "%a" (pp_bindings Base.pretty pp_elt) (Base.Map.bindings m)
  in
  let pp_stmt fmt stmt =
    let stmt_str = Format.asprintf "%a" Stmt.pretty stmt in
    let len = String.length stmt_str in
    Format.fprintf fmt "[sid:%d<%s>]" stmt.Cil_types.sid
      (if len < 10 then stmt_str else String.sub stmt_str 0 10 ^ "...")
  in
  Format.fprintf fmt
    "@[priority bases: %a@\n\
     default_hints: %a@\n\
     default_float_hints: %a@\n\
     default_hints_by_stmt: %a@\n\
     default_float_hints_by_stmt: %a@\n\
     hints_by_addr: %a@\n\
     float_hints_by_addr: %a@\n\
     hints_by_addr_by_stmt: %a@\n\
     float_hints_by_addr_by_stmt: %a@]"
    (pp_bindings pp_stmt Base.Set.pretty) (Stmt.Map.bindings wh.priority_bases)
    IntSet.pretty wh.default_hints
    FloatSet.pretty wh.default_float_hints
    (Pretty_utils.pp_list ~sep:",@ "
       (Pretty_utils.pp_pair ~sep:" -> " pp_stmt IntSet.pretty))
    (Stmt.Map.bindings wh.default_hints_by_stmt)
    (Pretty_utils.pp_list ~sep:",@ "
       (Pretty_utils.pp_pair ~sep:" -> " pp_stmt FloatSet.pretty))
    (Stmt.Map.bindings wh.default_float_hints_by_stmt)
    (Pretty_utils.pp_list ~sep:",@ "
       (Pretty_utils.pp_pair ~sep:" -> " Base.pretty IntSet.pretty))
    (Base.Map.bindings wh.hints_by_addr)
    (Pretty_utils.pp_list ~sep:",@ "
       (Pretty_utils.pp_pair ~sep:" -> " Base.pretty FloatSet.pretty))
    (Base.Map.bindings wh.float_hints_by_addr)
    (pp_bindings pp_stmt (pp_base_map IntSet.pretty))
    (Stmt.Map.bindings wh.hints_by_addr_by_stmt)
    (pp_bindings pp_stmt (pp_base_map FloatSet.pretty))
    (Stmt.Map.bindings wh.float_hints_by_addr_by_stmt)

let hints_from_keys stmt h =
  let int_hints_by_base =
    try
      let at_stmt = Stmt.Map.find stmt h.hints_by_addr_by_stmt in
      Base.Map.union
        (fun _b s1 s2 -> Some (IntSet.union s1 s2))
        at_stmt h.hints_by_addr
    with Not_found -> h.hints_by_addr
  in
  let float_hints_by_base =
    try
      let at_stmt = Stmt.Map.find stmt h.float_hints_by_addr_by_stmt in
      Base.Map.union
        (fun _b s1 s2 -> Some (FloatSet.union s1 s2))
        at_stmt h.float_hints_by_addr
    with Not_found -> h.float_hints_by_addr
  in
  let prio =
    try Stmt.Map.find stmt h.priority_bases
    with Not_found -> Base.Set.empty
  in
  let int_default =
    try
      let at_stmt = Stmt.Map.find stmt h.default_hints_by_stmt in
      IntSet.union h.default_hints at_stmt
    with Not_found -> h.default_hints
  in
  let float_default =
    try
      let at_stmt = Stmt.Map.find stmt h.default_float_hints_by_stmt in
      FloatSet.union h.default_float_hints at_stmt
    with Not_found -> h.default_float_hints
  in
  let int_hints_for_base b =
    try IntSet.union (Base.Map.find b int_hints_by_base) int_default
    with Not_found -> int_default
  in
  let float_hints_for_base b =
    try FloatSet.union (Base.Map.find b float_hints_by_base) float_default
    with Not_found -> float_default
  in
  prio, (fun b -> int_hints_for_base b, float_hints_for_base b)

let var_hints stmt prio_bases =
  let bases = Base.Set.filter (fun b -> not (Base.is_function b)) prio_bases in
  { empty with priority_bases = Stmt.Map.singleton stmt bases }

let num_hints stmto baseo hints =
  match stmto, baseo with
  | None, Some b -> (* Hints for a base at all statements *)
    { empty with hints_by_addr = Base.Map.singleton b hints }
  | Some stmt, Some b -> (* Hints for a base at a statement *)
    { empty with hints_by_addr_by_stmt = Stmt.Map.singleton stmt
                     (Base.Map.singleton b hints) }
  | Some stmt, None -> (* Hints for all bases and a given statement *)
    { empty with default_hints_by_stmt = Stmt.Map.singleton stmt hints }
  | None, None -> (* Hints for all bases and all statements *)
    { empty with default_hints = hints }

let float_hints stmto baseo hints =
  match stmto, baseo with
  | None, Some b -> (* Hints for a base at all statements *)
    { empty with float_hints_by_addr = Base.Map.singleton b hints }
  | Some stmt, Some b -> (* Hints for a base at a statement *)
    { empty with float_hints_by_addr_by_stmt = Stmt.Map.singleton stmt
                     (Base.Map.singleton b hints) }
  | Some stmt, None -> (* Hints for all bases and a given statement *)
    { empty with default_float_hints_by_stmt = Stmt.Map.singleton stmt hints }
  | None, None -> (* Hints for all bases and all statements *)
    { empty with default_float_hints = hints }

(* default set of hints. Depends on the machdep *)
let default () =
  let int_default = IntSet.of_list (List.map Z.of_int [-1;0;1]) in
  let max_32 = Floating_point.largest_finite_float_of Single in
  let positives = [ 0.0 ; 1.0 ; 10.0 ; 1e10 ; max_32 ; 1e80 ] in
  let negatives = List.map (fun x -> -.x) positives in
  let float_default = FloatSet.(union (of_list positives) (of_list negatives)) in
  join (num_hints None None int_default) (float_hints None None float_default)
