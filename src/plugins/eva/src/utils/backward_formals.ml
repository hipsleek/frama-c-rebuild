(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types


(* This function checks that an expression is guaranteed to evaluate in
   the same way before and after a call. We restrict ourselves to
   lvalues that are unreferenced locals or formals, because they cannot
   be changed by the callee. *)
let safe_argument expr =
  let is_safe_lval ~visitor:_ (lval : Eva_ast.lval) =
    match lval.node with
    | Var vi, NoOffset ->
      not (vi.vaddrof || Ast_types.has_qualifier "volatile" vi.vtype || vi.vglob)
    | _, _ -> false
  in
  let open Eva_ast_visitor.Observe in
  let observer = { default with observe_lval = is_safe_lval } in
  visit_exp ~neutral:true ~combine:(&&) observer expr

let written_formals kf =
  let module S = Cil_datatype.Varinfo.Set in
  match kf.fundec with
  | Declaration _ -> S.empty
  | Definition (fdec,  _) ->
    let add_addr_taken acc vi = if vi.vaddrof then S.add vi acc else acc in
    let referenced_formals =
      ref (List.fold_left add_addr_taken S.empty fdec.sformals)
    in
    let obj = object
      inherit Visitor.frama_c_inplace

      method! vinst i =
        begin match i with
          | Call (Some (Var vi, _), _, _, _)
          | Set ((Var vi, _), _, _) ->
            if Kernel_function.is_formal vi kf then
              referenced_formals := S.add vi !referenced_formals
          | _ -> ()
        end;
        Cil.SkipChildren
    end
    in
    ignore (Visitor.visitFramacFunction (obj :> Visitor.frama_c_visitor) fdec);
    !referenced_formals

module WrittenFormals =
  Kernel_function.Make_Table(Cil_datatype.Varinfo.Set)
    (struct
      let size = 17
      let dependencies = [Ast.self]
      let name = "Eva_utils.WrittenFormals"
    end)

let written_formals = WrittenFormals.memo written_formals
