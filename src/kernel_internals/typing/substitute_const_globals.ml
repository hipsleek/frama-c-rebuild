(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Syntactic substitution of globals, defined with the attribute 'const', with
   respective initializers. *)

open Cil_types
open Cil_datatype

let replace_by_zero exp =
  match Ast_types.unroll_node (Cil.typeOf exp) with
  | TInt ikind | TEnum { ekind = ikind } ->
    Cil.ChangeTo (Cil.kinteger ~loc:exp.eloc ikind 0)
  | TFloat fkind ->
    Cil.ChangeTo (Cil.kfloat ~loc:exp.eloc fkind 0.)
  | _ ->
    Cil.SkipChildren

class constGlobSubstVisitorClass : Cil.cilVisitor = object
  inherit Cil.nopCilVisitor

  val vi_to_init_opt = Varinfo.Hashtbl.create 7

  (* Visit globals and register only the association between globals with attribute
     'const' and respective initializers. *)
  method! vglob g =
    let rec is_arithmetic_type t =
      match t.tnode with
      | TArray (typ, _) -> is_arithmetic_type typ
      | TInt _ | TFloat _ | TEnum _ -> true
      | _ -> false
    in
    match g with
    | GVar (vi, _, _) ->
      (* Register in [vi_to_init_opt] the association between [vi] and its
         initializer [init_opt]. The latter is assumed to be an expression of
         literal constants only, as the lvals originally appearing in it have
         been substituted by the respective initializers in method [vexpr]. *)
      let register = function
        | GVar (vi, { init = init_opt }, _loc) as g ->
          Varinfo.Hashtbl.add vi_to_init_opt vi init_opt;
          g
        | _ ->
          (* Cannot happen as we treat only [GVar _] cases in the outer
             pattern matching. *)
          assert false
      in
      let typ = Ast_types.unroll_deep vi.vtype in
      if is_arithmetic_type typ && Ast_types.is_const typ
      then ChangeDoChildrenPost ([g], List.map register)
      else DoChildren
    | GFun _ -> DoChildren
    | _ -> SkipChildren

  (* Visit expressions and replace lvals, with a registered varinfo in
     [vi_to_init_opt], with respective initializing constant expressions. *)
  method! vexpr e =
    let loc = e.eloc in
    match e.enode with
    | Lval (Var vi, (NoOffset | Index _ as offset)) ->
      (* Support for variables and array, on arithmetic types only. *)
      begin match Varinfo.Hashtbl.find vi_to_init_opt vi with
        | None ->
          (* Since [vi] is a global, we replace it with the zero expression,
             i.e. the implicit initializer for such globals. *)
          replace_by_zero e
        | Some init ->
          let offset = Cil.constFoldOffset true offset in
          let rec find_replace current_offset current_init current_newexp =
            match current_init with
            | SingleInit si ->
              if Cil_datatype.OffsetStructEq.equal offset current_offset
              then Cil.ChangeTo (Cil.new_exp ~loc si.enode)
              else current_newexp
            | CompoundInit (ct, initl) ->
              (* We are dealing with an array: recursively [find_replace] among
                 its initializers. *)
              Cil.foldLeftCompound
                ~implicit:true
                ~doinit:(fun coffset cinit _ acc ->
                    find_replace
                      (Cil.addOffset coffset current_offset)
                      cinit
                      acc)
                ~ct
                ~initl
                ~acc:current_newexp
          in
          (match init, offset with
           | CInit i,_ -> find_replace NoOffset i (replace_by_zero e)
           | StrInit (Str s), Index (i,NoOffset) ->
             let l = Z.of_int (String.length s) in
             (match Cil.constFoldToInt i with
              | Some z when Z.leq Z.zero z && Z.lt z l ->
                let c = s.[Z.to_int z] in
                ChangeTo (Cil.new_exp ~loc (Const (CChr c)))
              | Some z when Z.equal z l ->
                ChangeTo (Cil.new_exp ~loc (Const (CChr '\000')))
              | Some _ | None -> DoChildren
             )
           | StrInit (Wstr l), Index(i,NoOffset) ->
             let len = Z.of_int (List.length l) in
             (match Cil.constFoldToInt i with
              | Some z when Z.leq Z.zero z && Z.lt z len ->
                let c = List.nth l (Z.to_int z) in
                ChangeTo
                  (Cil.kinteger64 ~loc ~kind:(Machine.wchar_kind()) (Z.of_int64 c))
              | Some z when Z.equal z len ->
                ChangeTo (Cil.kinteger64 ~loc ~kind:(Machine.wchar_kind()) Z.zero)
              | Some _ | None -> DoChildren)
           | StrInit _, _ -> DoChildren)
        | exception Not_found ->
          DoChildren
      end
    | _ ->
      DoChildren

  method! vtype _ = Cil.SkipChildren
  method! vspec _ = Cil.SkipChildren
  method! vcode_annot _ = Cil.SkipChildren
end

let constGlobSubstVisitor = new constGlobSubstVisitorClass
