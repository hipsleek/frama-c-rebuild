(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Logic_const
open Basic_blocks
open Cil_types

type kind = CPtr | Ptr | Data of typ
type action = Strip | Id
type param = string * kind * action
type proto = kind * param list
type 'a spec_gen = location -> typ -> term -> term -> term -> 'a

type pointed_expr_type =
  | Of_null of typ
  | Value_of of typ
  | No_pointed

let exp_type_of_pointed x =
  let no_cast = Cil.stripCasts x in
  if not (Ast_types.is_ptr (Cil.typeOf no_cast)) then
    match Cil.constFoldToInt x with
    | Some t when Z.(equal t (of_int 0)) ->
      Of_null (Ast_types.direct_pointed_type (Cil.typeOf x))
    | _ ->
      No_pointed
  else
    let xt = Ast_types.unroll_deep (Cil.typeOf no_cast) in
    let xt = Ast_types.remove_qualifiers_deep xt in
    Value_of (Ast_types.direct_pointed_type xt)

let unexpected = Options.fatal "Mem_utils: %s"

let mem2s_typing _ = function
  | [ dest ; src ; len ] ->
    (Ast_types.is_integral len) &&
    (Cil_datatype.Typ.equal dest src) &&
    (not (Ast_types.is_void dest)) &&
    (Cil.isCompleteType dest)
  | _ -> false

let mem2s_spec ~requires ~assigns ~ensures _t loc { svar = vi } =
  let (cdest, csrc, clen) = match Cil.getFormalsDecl vi with
    | [ dest ; src ; len ] -> dest, src, len
    | _ -> unexpected "ill-formed fundec in specification generation"
  in
  let t = cdest.vtype in
  let dest = cvar_to_tvar cdest in
  let src = cvar_to_tvar csrc in
  let len = cvar_to_tvar clen in
  let requires = requires loc t dest src len in
  let assigns  = assigns loc t dest src len in
  let ensures  = ensures loc t dest src len in
  make_funspec [make_behavior ~requires ~assigns ~ensures ()] ()

let pcopied_len_bytes ?loc p1 p2 bytes_len =
  plet_len_div_size ?loc p1.term_type bytes_len
    (punfold_all_elems_eq ?loc p1 p2)

let memcpy_memmove_common_requires loc _ dest src len =
  List.map new_predicate [
    { (pcorrect_len_bytes ~loc dest.term_type len)
      with pred_name = ["aligned_end"] } ;
    Logic_const.pand ~loc ~names:["valid_dest"] (
      pobject_pointer ~loc here_label dest,
      pvalid_len_bytes ~loc here_label dest len);
    Logic_const.pand ~loc ~names:["valid_read_src"] (
      pobject_pointer ~loc here_label src,
      pvalid_read_len_bytes ~loc here_label src len);
  ]

let memcpy_memmove_common_assigns loc t dest src len =
  let dest_range = new_identified_term (tunref_range_bytes_len ~loc dest len) in
  let src_range = new_identified_term(tunref_range_bytes_len ~loc src len) in
  let copy = dest_range, From [src_range] in
  let result = new_identified_term (tresult t) in
  let dest = new_identified_term dest in
  let res = result, From [dest] in
  Writes [ copy ; res ]

let presult_dest ?loc t dest =
  prel ?loc (Req, (tresult ?loc t), dest)

let memcpy_memmove_common_ensures name loc t dest src len =
  List.map (fun p -> Normal, new_predicate p) [
    { (pcopied_len_bytes ~loc dest src len) with pred_name = [name] } ;
    { (presult_dest ~loc t dest)           with pred_name = ["result"] }
  ]

module type Function = sig
  val name: string
  val prototype: unit -> proto
  val well_typed: typ option -> typ list -> bool
end

module Make (F: Function) =
struct
  let generate_function_type t =
    let to_type = function
      | CPtr -> Cil_const.mk_tptr (const_of t)
      | Ptr ->  Cil_const.mk_tptr t
      | Data t -> t
    in
    let ret, ps = F.prototype () in
    let ret = to_type ret in
    let ps = List.map (fun (name, kind, _) -> name, (to_type kind), []) ps in
    Cil_const.mk_tfun ret (Some ps) false

  let generate_prototype t =
    let ftype = generate_function_type t in
    let name = F.name ^ "_" ^ (string_of_typ t) in
    name, ftype

  let well_typed_call lval _fct args =
    let _, ps = F.prototype () in
    if List.length args <> List.length ps then false
    else
      let extract e = function
        | _, (CPtr | Ptr), _ -> exp_type_of_pointed e
        | _, Data _ , _ -> Value_of (Cil.typeOf e)
      in
      let lvt = Option.map Cil.typeOfLval lval in
      let pts = List.map2 extract args ps in
      let is_no_pointed = function No_pointed -> true | _ -> false in
      let the_typ = function
        | No_pointed -> assert false
        | Value_of t | Of_null t -> t
      in
      if List.exists is_no_pointed pts then false
      else F.well_typed lvt (List.map the_typ pts)

  let retype_args _ args =
    let _, ps = F.prototype () in
    if List.length args <> List.length ps then
      unexpected "trying to retype arguments on an ill-typed call"
    else
      let retype x = function
        | _, _, Strip -> Cil.stripCasts x
        | _, _, Id -> x
      in
      List.map2 retype args ps

  let key_from_call _ret _fct args =
    let _, ps = F.prototype () in
    match ps, args with
    | (_, (Ptr|CPtr), _) :: ps, fst :: args
      when List.(length ps = length args) ->
      begin match exp_type_of_pointed fst with
        | Value_of t -> t
        | _ ->
          unexpected "Mem_utils: trying to get key on an ill-typed call"
      end
    | _ ->
      unexpected "Mem_utils: trying to get key on an ill-typed call"
end
