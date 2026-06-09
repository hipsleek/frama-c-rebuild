(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

let emitter =
  let open Emitter in
  create
    "inline_stmt_contract" [Code_annot; Property_status]
    ~correctness:[] ~tuning:[]

let category = File.register_code_transformation_category "inline_stmt_contract"

let fresh_pred p =
  let v = new Visitor.frama_c_refresh (Project.current()) in
  Visitor.visitFramacPredicate v p

let nb_labels = ref 0

let new_label () =
  let res = Printf.sprintf "__fc_inline_contract_label_%d" !nb_labels in
  incr nb_labels; res

let propagate_status p1 p2 =
  match Property_status.get p1 with
  | Best (True, _) -> Property_status.emit emitter ~hyps:[p1] p2 True
  | _ ->
    begin
      let open Property_status in
      let propagate_status_p1 e p s =
        if Property.equal p p1 && s = True
           && not (Emitter.(Usable_emitter.equal e.emitter (get emitter)))
        then
          Property_status.emit emitter ~hyps:[p1] p2 True
      in
      Property_status.register_status_update_hook propagate_status_p1;
      let propagate_status_assertion e p s =
        if Property.equal p p2 && s = Property_status.True
           && not (Emitter.(Usable_emitter.equal e.emitter (get emitter)))
        then
          Property_status.emit emitter ~hyps:[p2] p1 True
      in
      Property_status.register_status_update_hook propagate_status_assertion
    end

let treat_behavior for_list kf stmt acc b =
  let loc = Cil_datatype.Stmt.loc stmt in
  let pand p1 p2 = Logic_const.pand ~loc (p1,p2.ip_content.tp_statement) in
  let assumes = List.fold_left pand Logic_const.ptrue b.b_assumes in
  let mk_annot p =
    let p = fresh_pred p in
    let p = Logic_const.toplevel_predicate p in
    Logic_const.new_code_annotation (AAssert (for_list,p))
  in
  let add_assert req =
    let p = Logic_const.pimplies ~loc (assumes, req.ip_content.tp_statement) in
    let ip_req = Property.ip_of_requires kf (Kstmt stmt) b req in
    let annot = mk_annot p in
    Annotations.add_code_annot emitter stmt annot;
    let ip_annot = Property.ip_of_code_annot_single kf stmt annot in
    propagate_status ip_req ip_annot
  in
  let treat_post_cond acc (k,p as postcond) =
    match k with
    | Normal ->
      if Logic_utils.is_trivially_true assumes then begin
        (b, postcond, mk_annot p.ip_content.tp_statement) :: acc
      end else begin
        (match List.filter (fun l -> not (Cil.is_case_label l)) stmt.labels with
         | [] -> stmt.labels <- Label (new_label(),loc,false) :: stmt.labels
         | _ -> ());
        let p =
          Logic_const.(
            pimplies ~loc
              (pat ~loc (assumes,StmtLabel (ref stmt)),
               p.ip_content.tp_statement))
        in
        (b, postcond, mk_annot p) :: acc
      end
    | _ -> acc
  in
  List.iter add_assert b.b_requires;
  List.fold_left treat_post_cond acc b.b_post_cond

let add_one_postcond kf orig_stmt new_stmt (b, post, assertion) =
  Annotations.add_code_annot emitter ~kf new_stmt assertion;
  let post = Property.ip_of_ensures kf (Kstmt orig_stmt) b post in
  let assertion = Property.ip_of_code_annot_single kf new_stmt assertion in
  propagate_status post assertion

class inline_stmt_contract =
  object(self)
    inherit Visitor.frama_c_inplace
    method! vstmt_aux s =
      let loc = Cil_datatype.Stmt.loc s in
      let kf = Option.get self#current_kf in
      match Annotations.code_annot ~filter:Logic_utils.is_contract s with
      | [ { annot_content = AStmtSpec (l,spec) } ] ->
        let bhvs = spec.spec_behavior in
        let posts = List.fold_left (treat_behavior l kf s) [] bhvs in
        (match posts with
         | [] -> Cil.DoChildren
         | _ ->
           let nop = Cil.mkStmtOneInstr ~valid_sid:true (Skip loc) in
           List.iter (add_one_postcond kf s nop) posts;
           let b = Cil.mkBlockNonScoping [s; nop] in
           let b = Cil.transient_block b in
           let res = Cil.mkStmt ~valid_sid:true (Block b) in
           Ast.mark_as_grown ();
           File.must_recompute_cfg (Option.get self#current_func);
           ChangeTo res
        )
      | [ _ ] -> Kernel.fatal "code annotation was expected to be a contract"
      | _ :: _ -> Kernel.fatal "a statement can only have one contract"
      | [] -> Cil.DoChildren

    method! vfunc _ = nb_labels:= 0; Cil.DoChildren

    method! vexpr _ = Cil.SkipChildren
    method! vlval _ = Cil.SkipChildren
    method! vtype _ = Cil.SkipChildren
    method! vspec _ = Cil.SkipChildren
    method! vcode_annot _ = Cil.SkipChildren
    method! vinst _ = Cil.SkipChildren

  end

let inline_stmt_contract ast =
  if Kernel.InlineStmtContracts.get () then
    begin
      let vis = new inline_stmt_contract in
      Visitor.visitFramacFileSameGlobals vis ast
    end

let () =
  let deps = [ (module Kernel.InlineStmtContracts: Parameter_sig.S) ] in
  File.add_code_transformation_after_cleanup ~deps category inline_stmt_contract
