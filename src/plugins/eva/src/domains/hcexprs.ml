(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Exp = Eva_ast.Exp
module Lval = Eva_ast.Lval

(* lvalues are never stored under a constructor [E]. *)
type unhashconsed_exprs = E of Exp.t | LV of Lval.t

(* The replacement of an lvalue by another term in an expression fails
   (raises an exception) if the height of the resulting expression exceeds
   this limit. *)
let height_limit = 8

exception NonExchangeable

type kill_type = Modified | Deleted

module E = struct
  include Datatype.Make (struct
      include Datatype.Serializable_undefined

      type t = unhashconsed_exprs
      let name = "Value.Symbolic_exprs.key"
      let reprs = List.map (fun e -> E e) Exp.reprs

      let structural_descr =
        Structural_descr.t_sum
          [| [| Exp.packed_descr |] ; [| Lval.packed_descr |] ; |]

      let equal a b = match a, b with
        | E e1, E e2 -> Exp.equal e1 e2
        | LV lv1, LV lv2 -> Lval.equal lv1 lv2
        | (E _ | LV _), _ -> false

      let compare a b = match a, b with
        | E e1, E e2 -> Exp.compare e1 e2
        | LV lv1, LV lv2 -> Lval.compare lv1 lv2
        | LV _, E _  -> -1
        | E _, LV _  -> 1

      let pretty fmt = function
        | E e ->   Format.fprintf fmt "%a" Exp.pretty e
        | LV lv -> Format.fprintf fmt "%a" Lval.pretty lv

      let hash = function
        | E e -> Exp.hash e
        | LV lv -> Lval.hash lv

      let copy c = c
    end)

  let replace kind ~late ~heir =
    let open Eva_ast.Rewrite in
    let rewrite_exp ~visitor (exp : Eva_ast.exp) =
      match exp.node with
      | Lval lval ->
        if Lval.equal lval late then heir else exp
      | StartOf lval | AddrOf lval ->
        if kind = Modified
        then exp
        else if Lval.equal lval late then raise NonExchangeable else exp
      | _ -> default.rewrite_exp ~visitor exp
    in
    let rewriter = { default with rewrite_exp } in
    Eva_ast.Rewrite.visit_exp rewriter
end

module HCE = struct
  module S =
    State_builder.Hashcons(E)
      (struct
        let dependencies = [Ast.self]
        let name = ""
        let initial_values = []
      end)

  include S

  let of_lval lv = hashcons (LV lv)

  let of_exp (exp : Eva_ast.exp) =
    match exp.node with
    | Lval lv -> of_lval lv
    | _ -> hashcons (E exp)

  let to_exp h = match get h with
    | E e -> e
    | LV lv -> Eva_ast.Build.lval lv

  let to_lval h = match get h with
    | E _ -> None
    | LV lv -> Some lv

  let is_lval h = match get h with
    | E _ -> false
    | LV _ -> true

  let type_of h = match get h with
    | E e -> e.typ
    | LV lv -> lv.typ

  let replace kind ~late ~heir h = match get h with
    | E e ->
      let e = E.replace kind ~late ~heir e in
      if Eva_ast.height_exp e > height_limit
      then raise NonExchangeable
      else of_exp e
    | LV lval -> if Lval.equal lval late then of_exp heir else h
end

module Hptmap_Info = struct
  let initial_values = []
  let dependencies = [ Ast.self ]
end

module HCESet = Hptset.Make (HCE) (Hptmap_Info)

type lvalues = {
  read : HCESet.t;
  addr : HCESet.t;
}

let empty_lvalues = { read = HCESet.empty; addr = HCESet.empty; }

let syntactic_lvalues expr =
  let rec gather (expr : Eva_ast.exp) lvalues =
    match expr.node with
    | Lval lv ->
      { lvalues with read = HCESet.add (HCE.of_lval lv) lvalues.read }
    | AddrOf lv | StartOf lv ->
      { lvalues with addr = HCESet.add (HCE.of_lval lv) lvalues.addr }
    | UnOp (_, e, _) | CastE (_, e) -> gather e lvalues
    | BinOp (_, e1, e2, _) -> gather e1 (gather e2 lvalues)
    | _ -> lvalues
  in
  gather expr empty_lvalues


module HCEToZone = struct

  let cache_prefix = "Value.Symbolic_exprs.K2Z"

  include Hptmap.Make (HCE) (Memory_zone) (Hptmap_Info)

  let is_included =
    let cache_name = cache_prefix ^ ".is_included" in
    let decide_fst _b _v1 = true in
    let decide_snd _b _v2 = false in
    let decide_both _ v1 v2 = Memory_zone.is_included v1 v2 in
    let decide_fast s t = if s == t then PTrue else PUnknown in
    binary_predicate
      (Hptmap_sig.PersistentCache cache_name) UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  let inter =
    let cache_name = cache_prefix ^ ".inter" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 = Some (Memory_zone.join v1 v2) in
    inter ~cache ~symmetric ~idempotent ~decide

  let union =
    let cache_name = cache_prefix ^ ".union" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 = Memory_zone.join v1 v2 in
    join ~cache ~symmetric ~idempotent ~decide

  let merge =
    let cache_name = cache_prefix ^ ".merge" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let decide _ _ v2 = v2 in
    join ~cache ~symmetric:false ~idempotent:true ~decide

  let merge ~into v = merge into v
end


module BaseToHCESet = struct

  include Hptmap.Make (Base.Base) (HCESet) (Hptmap_Info)

  let cache_prefix = "Value.Symbolic_exprs.B2K"

  let inter =
    let cache_name = cache_prefix ^ ".inter" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 =
      let s = HCESet.inter v1 v2 in
      if HCESet.is_empty s then None else Some s
    in
    inter ~cache ~symmetric ~idempotent ~decide

  let union =
    let cache_name = cache_prefix ^ ".union" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 = HCESet.union v1 v2 in
    join ~cache ~symmetric ~idempotent ~decide

  let find_default b m =
    try find b m
    with Not_found -> HCESet.empty

end
