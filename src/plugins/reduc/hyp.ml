(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let pred_opt_from_expr_state stmt e =
  try
    Value2acsl.lval_to_predicate stmt e
  with
  | Cvalue.V.Not_based_on_null ->
    Misc.not_implemented ~what:"Value not based on null";
    None
  | Misc.Not_implemented what ->
    Misc.not_implemented ~what;
    None

class hypotheses_visitor (env: Collect.env) = object(self)
  inherit Visitor.generic_frama_c_visitor (Visitor_behavior.inplace ())

  method! vstmt_aux stmt =
    let kf = Option.get (self#current_kf) in
    if Collect.should_annotate_stmt env stmt then begin
      let vars = Collect.get_relevant_vars_stmt env kf stmt in
      List.iter
        (fun e ->
           let p_opt = pred_opt_from_expr_state stmt e in
           Option.iter (Misc.assert_and_validate ~kf stmt) p_opt)
        vars
    end;
    Cil.DoChildren
end


let generate_hypotheses env =
  let visitor = new hypotheses_visitor env in
  Cil.visitCilFileSameGlobals (visitor :> Cil.cilVisitor) (Ast.get ())
