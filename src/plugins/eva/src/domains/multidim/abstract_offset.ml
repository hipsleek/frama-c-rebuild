(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lattice_bounds
open Top.Operators

type t =
  | NoOffset of Cil_types.typ
  | Index of Eva_ast.exp option * Int_val.t * Cil_types.typ * t
  | Field of Cil_types.fieldinfo * t

let rec pretty fmt = function
  | NoOffset _t -> ()
  | Field (fi, s) -> Format.fprintf fmt ".%s%a" fi.fname pretty s
  | Index (None, i, _t, s) ->
    Format.fprintf fmt "[%a]%a"
      Int_val.pretty i
      pretty s
  | Index (Some e, i, _t, s) ->
    Format.fprintf fmt "[%a%t%a]%a"
      Eva_ast.pp_exp e
      Unicode.pp_in_set
      Int_val.pretty i
      pretty s

let rec append o1 o2 =
  match o1 with
  | NoOffset _t -> o2
  | Field (fi, s) -> Field (fi, append s o2)
  | Index (e, i, t, s) -> Index (e, i, t, append s o2)

let add_index oracle base exp =
  let rec aux = function
    | NoOffset _ as o ->
      let idx = oracle exp in
      if Int_val.is_zero idx
      then `Value o
      else `Top (* Can't add index if not an array *)
    | Field (fi, sub) ->
      let+ sub' = aux sub in
      Field (fi, sub')
    | Index (e, i, t, (NoOffset _ as sub)) ->
      let idx' = Int_val.add i (oracle exp) in
      let e = match e with (* If i is singleton, we can use this as the index expression *)
        | None when Int_val.is_singleton i ->
          Some (Eva_ast.Build.integer ~ikind:IInt (Int_val.project_int i))
        | e -> e
      in
      let e' = Option.map (fun e -> Eva_ast.Build.(add e exp)) e in
      (* TODO: is idx inside bounds ? *)
      `Value (Index (e', idx', t, sub))
    | Index (e, i, t, sub) ->
      let+ sub' = aux sub in
      Index (e, i, t, sub')
  in
  aux base

let rec join o1 o2 =
  match o1, o2 with
  | NoOffset t, NoOffset t'
    when Bit_utils.type_compatible t t' ->
    `Value (NoOffset t)
  | Field (fi, sub1), Field (fi', sub2)
    when Cil_datatype.Fieldinfo.equal fi fi' ->
    let+ sub' = join sub1 sub2 in
    Field (fi, sub')
  | Index (e1, i1, t, sub1), Index (e2, i2, t', sub2)
    when Bit_utils.type_compatible t t' ->
    let e = match e1, e2 with
      | Some e1, Some e2 when Eva_ast.Exp.equal e1 e2 ->
        Some e1 (* keep expression only when equivalent from both offsets *)
      | _ -> None
    in
    let+ sub' = join sub1 sub2 in
    Index (e, Int_val.join i1 i2, t, sub')
  | _ -> `Top

let array_bounds array_size =
  (* TODO: handle undefined sizes and not const foldable sizes *)
  match array_size with
  | None -> None
  | Some size_exp ->
    match Cil.constFoldToInt size_exp with
    | None -> None
    | Some size when Z.gt size 0z -> Some (0z, Z.pred size)
    | Some _ -> None

let array_range array_size =
  Option.map
    (fun (l,u) -> Int_val.inject_range (Some l) (Some u))
    (array_bounds array_size)

let assert_valid_index idx size =
  match array_range size with
  | Some range when Int_val.is_included idx range -> `Value ()
  | _ -> `Top

let of_var_address vi =
  NoOffset vi.Cil_types.vtype

let rec of_eva_offset (oracle : Eva_ast.exp -> Int_val.t) base_typ = function
  | Eva_ast.NoOffset -> `Value (NoOffset base_typ)
  | Field (fi, sub) ->
    if Ast_types.has_qualifier "volatile" fi.ftype then
      `Top
    else
      let+ sub' = of_eva_offset oracle fi.ftype sub in
      Field (fi, sub')
  | Index (exp, sub) ->
    match Ast_types.unroll_node base_typ with
    | TArray (elem_typ, array_size) ->
      let idx = oracle exp in
      let+ () = assert_valid_index idx array_size
      and+ sub' = of_eva_offset oracle elem_typ sub in
      Index (Some exp, idx, elem_typ, sub')
    | _ -> assert false

let rec of_int_val ~base_typ ~typ ival =
  if Int_val.is_zero ival && Bit_utils.type_compatible base_typ typ then
    `Value (NoOffset typ)
  else
    match Ast_types.unroll_node base_typ with
    | TArray (elem_typ, array_size) ->
      let* range, rem =
        try
          let elem_size = Z.of_int (Cil.bitsSizeOf elem_typ) in
          if Z.is_zero elem_size then (* array of elements of size 0 *)
            if Int_val.is_zero ival then (* the whole range is valid *)
              let+ range = Top.of_option (array_range array_size) in
              range, ival
            else (* Non-zero offset cannot represent anything here *)
              `Top
          else
            let range = Int_val.scale_div ~pos:true elem_size ival
            and rem = Int_val.scale_rem ~pos:true elem_size ival in
            `Value (range, rem)
        with Cil.SizeOfError (_,_) ->
          (* Cil.bitsSizeOf can raise an exception when elements are
              themselves array of execution-time size *)
          if Int_val.is_zero ival then
            `Value (ival, ival)
          else
            `Top
      in
      let+ () = assert_valid_index range array_size
      and+ sub' = of_int_val ~base_typ:elem_typ ~typ rem in
      Index (None, range, elem_typ, sub')

    | TComp ci ->
      if not ci.cstruct then
        (* Ignore unions for now *)
        `Top
      else
        let rec find_field = function
          | [] -> `Top
          | fi :: q ->
            let offset, width = Cil.fieldBitsOffset fi in
            let l = Z.(of_int offset) in
            let u = Z.(pred (add l (of_int width))) in
            let matches =
              if width = 0 then
                Int_val.(equal ival (inject_singleton l))
              else
                let range = Int_val.inject_range (Some l) (Some u) in
                Int_val.is_included ival range
            in
            if matches then
              if Ast_types.has_qualifier "volatile" fi.ftype then
                `Top
              else
                let sub_ival = Int_val.add_singleton (Z.neg l) ival in
                let+ sub' = of_int_val ~base_typ:fi.ftype ~typ sub_ival in
                Field (fi, sub')
            else
              find_field q
        in
        find_field (Option.value ~default:[] ci.cfields)

    | _ -> `Top

let of_ival ~base_typ ~typ ival =
  match Ival.project_int_val ival with
  | Some ival -> of_int_val ~base_typ ~typ ival
  | None -> `Top

let index_of_term array_size t = (* Exact constant ranges *)
  match t.Cil_types.term_node with
  | TConst (Integer (v, _)) -> `Value (Int_val.inject_singleton v)
  | Trange (l,u) ->
    let eval bound = function
      | None -> Top.of_option bound
      | Some { Cil_types.term_node=TConst (Integer (v, _)) } -> `Value v
      | Some _ -> `Top
    in
    let bounds = array_bounds array_size in
    let lb = Option.map fst bounds and ub = Option.map snd bounds in
    let+ l' = eval lb l and+ u' = eval ub u in
    Int_val.inject_range (Some l') (Some u')
  | _ -> `Top

let rec of_term_offset base_typ = function
  | Cil_types.TNoOffset -> `Value (NoOffset base_typ)
  | TField (fi, sub) ->
    if Ast_types.has_qualifier "volatile" fi.ftype then
      `Top
    else
      let+ sub' = of_term_offset fi.ftype sub in
      Field (fi, sub')
  | TIndex (index, sub) ->
    begin match Ast_types.unroll_node base_typ with
      | TArray (elem_typ, array_size) ->
        let* idx = index_of_term array_size index in
        let+ () = assert_valid_index idx array_size
        and+ sub' = of_term_offset elem_typ sub in
        Index (None, idx, elem_typ, sub')
      | _ -> assert false
    end
  | _ -> `Top

let rec is_singleton = function
  | NoOffset _ -> true
  | Field (_fi, sub) -> is_singleton sub
  | Index (e, ival, _elem_typ, sub) ->
    (Option.is_some e || Int_val.is_singleton ival) && is_singleton sub

let references =
  let rec aux acc = function
    | NoOffset _ -> acc
    | Field (_, sub) | Index (None, _, _, sub) -> aux acc sub
    | Index (Some e, _, _, sub) ->
      let r = Eva_ast.vars_in_exp e in
      let acc = Cil_datatype.Varinfo.Set.union r acc in
      aux acc sub
  in
  aux Cil_datatype.Varinfo.Set.empty
